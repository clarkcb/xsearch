# -*- coding: utf-8 -*-
"""
################################################################################
#
# searchresult_test.py
#
# class SearchResultTest: testing of SearchResult and SearchResultFormatter
#
################################################################################
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pyfind import FileType, FileResult
from pysearch import Color, SearchResult, SearchResultFormatter, SearchSettings, XSEARCHPATH


def get_formatter(colorize: bool = True):
    return SearchResultFormatter(SearchSettings(colorize=colorize))


class SearchResultTest(unittest.TestCase):
    cssearch_path = os.path.join(XSEARCHPATH, 'csharp/CsSearch/CsSearch')

    def test_single_line_search_result(self):
        formatter = get_formatter(colorize=False)
        pattern = "Search"
        file_name = 'Searcher.cs'
        fr = FileResult(path=self.cssearch_path, file_name=file_name, file_type=FileType.CODE)
        line_num = 10
        match_start_index = 15
        match_end_index = 23
        line = "\tpublic class Searcher\n"
        lines_before = []
        lines_after = []
        search_result = SearchResult(pattern=pattern,
                                     file=fr,
                                     line_num=line_num,
                                     match_start_index=match_start_index,
                                     match_end_index=match_end_index,
                                     line=line,
                                     lines_before=lines_before,
                                     lines_after=lines_after)
        expected_output = "{0:s}: {1:d}: [{2:d}:{3:d}]: {4:s}".format(os.path.join(self.cssearch_path, file_name),
                                                                      line_num, match_start_index, match_end_index,
                                                                      line.strip())
        output = formatter.format(search_result)
        self.assertEqual(expected_output, output)

    def test_single_line_longer_than_max_length_search_result(self):
        formatter = SearchResultFormatter(SearchSettings(colorize=False, max_line_length=100))
        pattern = 'maxlen'
        file_name = 'maxlen.txt'
        fr = FileResult(path='.', file_name=file_name, filetype=FileType.TEXT)
        line_num = 1
        match_start_index = 53
        match_end_index = 59
        line = '0123456789012345678901234567890123456789012345678901' + \
               'maxlen' + \
               '8901234567890123456789012345678901234567890123456789'
        lines_before = []
        lines_after = []
        search_result = SearchResult(pattern=pattern,
                                     file=fr,
                                     line_num=line_num,
                                     match_start_index=match_start_index,
                                     match_end_index=match_end_index,
                                     line=line,
                                     lines_before=lines_before,
                                     lines_after=lines_after)
        expected_line = '...89012345678901234567890123456789012345678901' + \
                        'maxlen89012345678901234567890123456789012345678901...'
        expected_output = "{0:s}: {1:d}: [{2:d}:{3:d}]: {4:s}".format(os.path.join('.', file_name),
                                                                      line_num, match_start_index, match_end_index,
                                                                      expected_line)
        output = formatter.format(search_result)
        self.assertEqual(expected_output, output)

    def test_single_line_longer_colorize_search_result(self):
        formatter = SearchResultFormatter(SearchSettings(colorize=True, max_line_length=100))
        pattern = 'maxlen'
        file_name = 'maxlen.txt'
        fr = FileResult(path='.', file_name=file_name, file_type=FileType.TEXT)
        line_num = 10
        match_start_index = 53
        match_end_index = 59
        line = '0123456789012345678901234567890123456789012345678901' + \
               'maxlen' + \
               '8901234567890123456789012345678901234567890123456789'
        lines_before = []
        lines_after = []
        search_result = SearchResult(pattern=pattern,
                                     file=fr,
                                     line_num=line_num,
                                     match_start_index=match_start_index,
                                     match_end_index=match_end_index,
                                     line=line,
                                     lines_before=lines_before,
                                     lines_after=lines_after)
        expected_line = '...89012345678901234567890123456789012345678901' + \
                        Color.GREEN + 'maxlen' + Color.RESET + \
                        '89012345678901234567890123456789012345678901...'
        expected_output = "{0:s}: {1:d}: [{2:d}:{3:d}]: {4:s}".format(os.path.join('.', file_name),
                                                                      line_num, match_start_index, match_end_index,
                                                                      expected_line)
        output = formatter.format(search_result)
        self.assertEqual(expected_output, output)

    def test_binary_file_search_result(self):
        formatter = get_formatter(colorize=True)
        pattern = "Search"
        file_name = 'Searcher.exe'
        fr = FileResult(path=self.cssearch_path, file_name=file_name, file_type=FileType.BINARY)
        line_num = 0
        match_start_index = 0
        match_end_index = 0
        line = ''
        lines_before = []
        lines_after = []
        search_result = SearchResult(pattern=pattern,
                                     file=fr,
                                     line_num=line_num,
                                     match_start_index=match_start_index,
                                     match_end_index=match_end_index,
                                     line=line,
                                     lines_before=lines_before,
                                     lines_after=lines_after)
        expected_output = "{0:s} matches at [{1:d}:{2:d}]".format(os.path.join(self.cssearch_path, file_name),
                                                                  match_start_index, match_end_index)
        output = formatter.format(search_result)
        self.assertEqual(expected_output, output)

    def test_multi_line_searchresult(self):
        formatter = get_formatter(colorize=False)
        pattern = "Search"
        file_name = 'Searcher.cs'
        fr = FileResult(path=self.cssearch_path, file_name=file_name, file_type=FileType.TEXT)
        line_num = 10
        match_start_index = 15
        match_end_index = 23
        line = "\tpublic class Searcher\n"
        lines_before = ["namespace CsSearch\n", "{\n"]
        lines_after = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"]
        search_result = SearchResult(pattern=pattern, file=fr,
                                     line_num=line_num, match_start_index=match_start_index,
                                     match_end_index=match_end_index, line=line, lines_before=lines_before,
                                     lines_after=lines_after)
        expectedoutput = "================================================================================\n" + \
            '{0:s}: {1:d}: [{2:d}:{3:d}]\n'.format(os.path.join(self.cssearch_path, file_name), line_num, match_start_index, match_end_index) + \
            """--------------------------------------------------------------------------------
   8 | namespace CsSearch
   9 | {
> 10 | \tpublic class Searcher
  11 | \t{
  12 | \t\tprivate readonly FileTypes _fileTypes;
"""
        output = formatter.format(search_result)
        self.assertEqual(expectedoutput, output)


if __name__ == '__main__':
    unittest.main()
