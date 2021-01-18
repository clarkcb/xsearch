# -*- coding: utf-8 -*-
###############################################################################
#
# searchresult.py
#
# class SearchResult: encapsulates a search result
#
###############################################################################
from io import StringIO
from typing import List

from .color import Color
from .searchfile import SearchFile
from .searchsettings import SearchSettings


def _atmost_before_index(s, maxlen, start_index):
    if start_index >= maxlen:
        return '...' + s[start_index - maxlen - 3:start_index]
    else:
        return s[:start_index]


def _atmost_after_index(s, maxlen, start_index):
    if len(s[start_index:]) > maxlen:
        return s[start_index:maxlen - 3] + '...'
    else:
        return s[start_index:]


def strip_newlines(s):
    return s.rstrip("\r\n")


class SearchResult(object):
    """encapsulates a search result"""

    __slots__ = ['pattern', 'file', 'linenum', 'line', 'contained', 'lines_before', 'lines_after',
                 'match_start_index', 'match_end_index']

    def __init__(self, pattern: str = '', file: SearchFile = None, linenum: int = 0, line: str = '',
                 contained: str = '', lines_before: List[str] = None, lines_after: List[str] = None,
                 match_start_index: int = 0, match_end_index: int = 0):
        self.pattern = pattern
        self.file = file
        self.linenum = linenum
        self.line = line
        self.contained = contained
        self.lines_before = lines_before
        self.lines_after = lines_after
        self.match_start_index = match_start_index
        self.match_end_index = match_end_index

    @property
    def sortkey(self):
        path = self.file.path.lower()
        filename = self.file.filename.lower()
        return path, filename, self.linenum, self.match_start_index


class SearchResultFormatter(object):
    """provides formatting of SearchResult instances"""
    SEPARATOR_LEN = 80

    def __init__(self, settings: SearchSettings):
        self.settings = settings

    @staticmethod
    def colorize(s: str, match_start_index: int, match_end_index: int) -> str:
        return s[0:match_start_index] + Color.GREEN + \
               s[match_start_index:match_end_index] + \
               Color.RESET + s[match_end_index:]

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

        # track where match start and end indices end up (changing to zero-indexed)
        match_start_index = result.match_start_index - 1 - leading_ws_count
        match_end_index = match_start_index + match_length

        # If longer than maxlinelength, walk out from match indices
        if formatted_length > self.settings.maxlinelength:
            line_start_index = match_start_index
            line_end_index = line_start_index + match_length
            # NOTE: these are tracked so we can colorize at correct indices
            match_start_index = 0
            match_end_index = match_length

            while line_end_index > formatted_length - 1:
                line_start_index -= 1
                line_end_index -= 1
                match_start_index += 1
                match_end_index += 1

            formatted_length = line_end_index - line_start_index
            while formatted_length < self.settings.maxlinelength:
                if line_start_index > 0:
                    line_start_index -= 1
                    match_start_index += 1
                    match_end_index += 1
                    formatted_length = line_end_index - line_start_index
                if formatted_length < self.settings.maxlinelength and \
                        line_end_index < max_line_end_index:
                    line_end_index += 1
                formatted_length = line_end_index - line_start_index
            formatted = formatted[line_start_index:line_end_index]
            if line_start_index > 2:
                formatted = '...' + formatted[3:]
            if line_end_index < max_line_end_index - 3:
                formatted = formatted[:-3] + '...'
        if self.settings.colorize:
            formatted = self.colorize(formatted, match_start_index, match_end_index)
        return formatted

    def __singleline_format(self, result: SearchResult) -> str:
        sio = StringIO()
        sio.write(str(result.file))
        if result.linenum and result.line:
            sio.write(': {0:d}: [{1:d}:{2:d}]'.format(result.linenum,
                                                      result.match_start_index,
                                                      result.match_end_index))
            sio.write(': {0:s}'.format(self.__format_matching_line(result)))
        else:
            sio.write(' matches at [{0:d}:{1:d}]'.format(
                result.match_start_index, result.match_end_index))
        s = sio.getvalue()
        sio.close()
        return s

    def __linenum_padding(self, result: SearchResult) -> int:
        max_linenum = result.linenum + len(result.lines_after)
        return len(str(max_linenum))

    def __multiline_format(self, result: SearchResult) -> str:
        sio = StringIO()
        sio.write('{0}\n{1}:'.format('=' * self.SEPARATOR_LEN, str(result.file)))
        sio.write(' {0:d}: [{1:d}:{2:d}]'.format(result.linenum, result.match_start_index,
                                                 result.match_end_index))
        if result.contained:
            sio.write(': {0}'.format(result.contained))
        sio.write('\n{0}\n'.format('-' * self.SEPARATOR_LEN))
        line_format = ' {0:>' + str(self.__linenum_padding(result)) + 'd} | {1:s}\n'
        current_linenum = result.linenum
        if result.lines_before:
            current_linenum -= len(result.lines_before)
            for line_before in result.lines_before:
                sio.write(' ' + line_format.format(current_linenum,
                                                   strip_newlines(line_before)))
                current_linenum += 1
        if self.settings.colorize:
            line = self.colorize(result.line, result.match_start_index - 1,
                                 result.match_end_index - 1)
        else:
            line = result.line
        sio.write('>' + line_format.format(result.linenum,
                                           strip_newlines(line)))
        if result.lines_after:
            current_linenum = result.linenum + 1
            for line_after in result.lines_after:
                sio.write(' ' + line_format.format(current_linenum,
                                                   strip_newlines(line_after)))
                current_linenum += 1
        s = sio.getvalue()
        sio.close()
        return s

    def format(self, result: SearchResult) -> str:
        if result.lines_before or result.lines_after:
            return self.__multiline_format(result)
        else:
            return self.__singleline_format(result)
