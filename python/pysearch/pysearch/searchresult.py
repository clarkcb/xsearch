# -*- coding: utf-8 -*-
"""
###############################################################################
#
# searchresult.py
#
# class SearchResult: encapsulates a search result
#
###############################################################################
"""
from io import StringIO
from typing import List

from pyfind import ConsoleColor, FileResult, FileResultFormatter, FileResultSorter, SortBy

from .searchsettings import SearchSettings


def _at_most_before_index(s, max_len, start_index):
    if start_index >= max_len:
        return '...' + s[start_index - max_len - 3:start_index]
    else:
        return s[:start_index]


def _at_most_after_index(s, max_len, start_index):
    if len(s[start_index:]) > max_len:
        return s[start_index:max_len - 3] + '...'
    else:
        return s[start_index:]


def strip_newlines(s):
    return s.rstrip("\r\n")


class SearchResult(object):
    """encapsulates a search result"""

    __slots__ = ['pattern', 'file', 'line_num', 'line', 'contained', 'lines_before', 'lines_after',
                 'match_start_index', 'match_end_index']

    def __init__(self, pattern: str = '', file: FileResult = None, line_num: int = 0, line: str = '',
                 contained: str = '', lines_before: List[str] = None, lines_after: List[str] = None,
                 match_start_index: int = 0, match_end_index: int = 0):
        self.pattern = pattern
        self.file = file
        self.line_num = line_num
        self.line = line
        self.contained = contained
        self.lines_before = lines_before
        self.lines_after = lines_after
        self.match_start_index = match_start_index
        self.match_end_index = match_end_index

    @property
    def sort_key(self):
        path = str(self.file.path.parent).lower()
        filename = self.file.path.name.lower()
        return path, filename, self.line_num, self.match_start_index


class SearchResultFormatter(object):
    """provides formatting of SearchResult instances"""
    SEPARATOR_LEN = 80

    def __init__(self, settings: SearchSettings):
        self.settings = settings
        self.file_formatter = FileResultFormatter(settings)
        if settings.colorize:
            self.format_line = self.__format_line_with_color

    def __format_line_with_color(self, line: str) -> str:
        """format a line, highlighting matches with color"""
        formatted_line = str(line)
        for p in self.settings.search_patterns:
            match = p.search(formatted_line)
            if match:
                formatted_line = FileResultFormatter.colorize(
                    formatted_line, match.start(), match.end())
                break
        return formatted_line

    def format_line(self, line: str) -> str:
        """format a line, just returns the line by default but can be
           redefined to point to __format_line_with_color() if settings
           require colorization"""
        return str(line)

    def __format_matching_line(self, result: SearchResult) -> str:
        if not result.line or not result.line.strip():
            return ''
        formatted = result.line
        leading_ws_count = 0
        whitespace_chars = ' \t\n\r'
        while formatted[leading_ws_count] in whitespace_chars:
            leading_ws_count += 1
        formatted = result.line.strip()
        formatted_length = len(formatted)
        max_line_end_index = formatted_length - 1
        match_length = result.match_end_index - result.match_start_index

        # track where match start and end indices end up (changing to
        # zero-indexed)
        match_start_index = result.match_start_index - 1 - leading_ws_count
        match_end_index = match_start_index + match_length

        # If longer than max_line_length, walk out from match indices
        if formatted_length > self.settings.max_line_length:
            line_start_index = match_start_index
            line_end_index = line_start_index + match_length
            # NOTE: these are tracked so that we can colorize at correct indices
            match_start_index = 0
            match_end_index = match_length

            while line_end_index > formatted_length - 1:
                line_start_index -= 1
                line_end_index -= 1
                match_start_index += 1
                match_end_index += 1

            formatted_length = line_end_index - line_start_index
            while formatted_length < self.settings.max_line_length:
                if line_start_index > 0:
                    line_start_index -= 1
                    match_start_index += 1
                    match_end_index += 1
                    formatted_length = line_end_index - line_start_index
                if formatted_length < self.settings.max_line_length and \
                        line_end_index < max_line_end_index:
                    line_end_index += 1
                formatted_length = line_end_index - line_start_index
            formatted = formatted[line_start_index:line_end_index]
            if line_start_index > 2:
                formatted = '...' + formatted[3:]
            if line_end_index < max_line_end_index - 3:
                formatted = formatted[:-3] + '...'
        if self.settings.colorize:
            formatted = FileResultFormatter.colorize(
                formatted, match_start_index, match_end_index, self.settings.line_color)
        return formatted

    def __single_line_format(self, result: SearchResult) -> str:
        sio = StringIO()
        sio.write(self.file_formatter.format_file_result(result.file))
        if result.line_num and result.line:
            sio.write(': {0:d}: [{1:d}:{2:d}]'.format(result.line_num,
                                                      result.match_start_index,
                                                      result.match_end_index))
            sio.write(': {0:s}'.format(self.__format_matching_line(result)))
        else:
            sio.write(' matches at [{0:d}:{1:d}]'.format(
                result.match_start_index, result.match_end_index))
        s = sio.getvalue()
        sio.close()
        return s

    def __line_num_padding(self, result: SearchResult) -> int:
        max_line_num = result.line_num + len(result.lines_after)
        return len(str(max_line_num))

    def __multi_line_format(self, result: SearchResult) -> str:
        sio = StringIO()
        sio.write('{0}\n{1}:'.format(
            '=' * self.SEPARATOR_LEN, self.file_formatter.format_file_result(result.file)))
        sio.write(' {0:d}: [{1:d}:{2:d}]'.format(result.line_num, result.match_start_index,
                                                 result.match_end_index))
        if result.contained:
            sio.write(': {0}'.format(result.contained))
        sio.write('\n{0}\n'.format('-' * self.SEPARATOR_LEN))
        line_format = ' {0:>' + \
                      str(self.__line_num_padding(result)) + 'd} | {1:s}\n'
        current_line_num = result.line_num
        if result.lines_before:
            current_line_num -= len(result.lines_before)
            for line_before in result.lines_before:
                sio.write(' ' + line_format.format(current_line_num,
                                                   strip_newlines(line_before)))
                current_line_num += 1
        if self.settings.colorize:
            line = FileResultFormatter.colorize(result.line, result.match_start_index - 1,
                                                result.match_end_index - 1)
        else:
            line = result.line
        sio.write('>' + line_format.format(result.line_num,
                                           strip_newlines(line)))
        if result.lines_after:
            current_line_num = result.line_num + 1
            for line_after in result.lines_after:
                sio.write(' ' + line_format.format(current_line_num,
                                                   strip_newlines(line_after)))
                current_line_num += 1
        s = sio.getvalue()
        sio.close()
        return s

    def format(self, result: SearchResult) -> str:
        if result.lines_before or result.lines_after:
            return self.__multi_line_format(result)
        else:
            return self.__single_line_format(result)


class SearchResultSorter(object):
    """provides sorting of SearchResult instances"""

    def __init__(self, settings: SearchSettings):
        self.settings = settings
        self.file_sorter = FileResultSorter(settings)

    def key_by_search_fields(self, r: SearchResult) -> list:
        return [r.line_num, r.match_start_index, r.match_end_index]

    def key_by_file_path(self, r: SearchResult) -> list:
        key = []
        if r.file:
            key.extend(self.file_sorter.key_by_file_path(r.file))
        key.extend(self.key_by_search_fields(r))
        return key

    def key_by_file_name(self, r: SearchResult) -> list:
        key = []
        if r.file:
            key.extend(self.file_sorter.key_by_file_name(r.file))
        key.extend(self.key_by_search_fields(r))
        return key

    def key_by_file_size(self, r: SearchResult) -> list:
        key = []
        if r.file:
            key.extend(self.file_sorter.key_by_file_size(r.file))
        key.extend(self.key_by_search_fields(r))
        return key

    def key_by_file_type(self, r: SearchResult) -> list:
        key = []
        if r.file:
            key.extend(self.file_sorter.key_by_file_type(r.file))
        key.extend(self.key_by_search_fields(r))
        return key

    def key_by_last_mod(self, r: SearchResult) -> list:
        key = []
        if r.file:
            key.extend(self.file_sorter.key_by_last_mod(r.file))
        key.extend(self.key_by_search_fields(r))
        return key

    def get_sort_key_function(self) -> callable:
        """Get the sort key function based on the settings."""
        match self.settings.sort_by:
            case SortBy.FILEPATH:
                return self.key_by_file_path
            case SortBy.FILENAME:
                return self.key_by_file_name
            case SortBy.FILESIZE:
                return self.key_by_file_size
            case SortBy.FILETYPE:
                return self.key_by_file_type
            case SortBy.LASTMOD:
                return self.key_by_last_mod
            case _:
                return self.key_by_file_path

    def sort(self, results: list[SearchResult]) -> list[SearchResult]:
        """Sort the given list of SearchResult instances."""
        sort_key_func = self.get_sort_key_function()
        return sorted(results, key=sort_key_func,
                      reverse=self.settings.sort_descending)
