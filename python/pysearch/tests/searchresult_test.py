# -*- coding: utf-8 -*-
################################################################################
#
# searchresult_test.py
#
# class SearchResultTest: testing of SearchResult and SearchResultFormatter
#
################################################################################
import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from pysearch import Color, FileType, SearchFile, SearchResult, SearchResultFormatter, SearchSettings, XSEARCHPATH


class SearchResultTest(unittest.TestCase):

    cssearch_path = "%s/csharp/CsSearch/CsSearch" % XSEARCHPATH

    def test_singleline_searchresult(self):
        formatter = SearchResultFormatter(SearchSettings(colorize=False))
        pattern = "Search"
        filename = 'Searcher.cs'
        sf = SearchFile(path=self.cssearch_path, filename=filename, filetype=FileType.CODE)
        linenum = 10
        match_start_index = 15
        match_end_index = 23
        line = "\tpublic class Searcher\n"
        linesbefore = []
        linesafter = []
        searchresult = SearchResult(pattern=pattern,
                                    file=sf,
                                    linenum=linenum,
                                    match_start_index=match_start_index,
                                    match_end_index=match_end_index,
                                    line=line,
                                    lines_before=linesbefore,
                                    lines_after=linesafter)
        expectedoutput = "{}: {}: [{}:{}]: {}".format(os.path.join(self.cssearch_path, filename), linenum,
            match_start_index, match_end_index, line.strip())
        output = formatter.format(searchresult)
        self.assertEqual(expectedoutput, output)

    def test_singleline_longer_than_maxlength_searchresult(self):
        formatter = SearchResultFormatter(SearchSettings(colorize=False, maxlinelength=100))
        pattern = 'maxlen'
        filename = 'maxlen.txt'
        sf = SearchFile(path='.', filename=filename, filetype=FileType.TEXT)
        linenum = 1
        match_start_index = 53
        match_end_index = 59
        line = '0123456789012345678901234567890123456789012345678901' + \
               'maxlen' + \
               '8901234567890123456789012345678901234567890123456789'
        linesbefore = []
        linesafter = []
        searchresult = SearchResult(pattern=pattern,
                                    file=sf,
                                    linenum=linenum,
                                    match_start_index=match_start_index,
                                    match_end_index=match_end_index,
                                    line=line,
                                    lines_before=linesbefore,
                                    lines_after=linesafter)
        expectedline = '...89012345678901234567890123456789012345678901' + \
                       'maxlen89012345678901234567890123456789012345678901...'
        expectedoutput = "{}: {}: [{}:{}]: {}".format(os.path.join('.', filename), linenum,
            match_start_index, match_end_index, expectedline)
        output = formatter.format(searchresult)
        self.assertEqual(expectedoutput, output)

    def test_singleline_longer_colorize_searchresult(self):
        formatter = SearchResultFormatter(SearchSettings(colorize=True, maxlinelength=100))
        pattern = 'maxlen'
        filename = 'maxlen.txt'
        sf = SearchFile(path='.', filename=filename, filetype=FileType.TEXT)
        linenum = 10
        match_start_index = 53
        match_end_index = 59
        line = '0123456789012345678901234567890123456789012345678901' + \
               'maxlen' + \
               '8901234567890123456789012345678901234567890123456789'
        linesbefore = []
        linesafter = []
        searchresult = SearchResult(pattern=pattern,
                                    file=sf,
                                    linenum=linenum,
                                    match_start_index=match_start_index,
                                    match_end_index=match_end_index,
                                    line=line,
                                    lines_before=linesbefore,
                                    lines_after=linesafter)
        expectedline = '...89012345678901234567890123456789012345678901' + \
                       Color.GREEN + 'maxlen' + Color.RESET + \
                       '89012345678901234567890123456789012345678901...'
        expectedoutput = "{}: {}: [{}:{}]: {}".format(os.path.join('.', filename), linenum,
            match_start_index, match_end_index, expectedline)
        output = formatter.format(searchresult)
        self.assertEqual(expectedoutput, output)

    def test_binaryfile_searchresult(self):
        formatter = SearchResultFormatter(SearchSettings())
        pattern = "Search"
        filename = 'Searcher.exe'
        sf = SearchFile(path=self.cssearch_path, filename=filename, filetype=FileType.BINARY)
        linenum = 0
        match_start_index = 0
        match_end_index = 0
        line = ''
        linesbefore = []
        linesafter = []
        searchresult = SearchResult(pattern=pattern,
                                    file=sf,
                                    linenum=linenum,
                                    match_start_index=match_start_index,
                                    match_end_index=match_end_index,
                                    line=line,
                                    lines_before=linesbefore,
                                    lines_after=linesafter)
        expectedoutput = "{} matches at [{}:{}]".format(os.path.join(self.cssearch_path, filename),
                                                        match_start_index, match_end_index)
        output = formatter.format(searchresult)
        self.assertEqual(expectedoutput, output)

    def test_multiline_searchresult(self):
        formatter = SearchResultFormatter(SearchSettings(colorize=False))
        pattern = "Search"
        filename = 'Searcher.cs'
        sf = SearchFile(path=self.cssearch_path, filename=filename, filetype=FileType.TEXT)
        linenum = 10
        match_start_index = 15
        match_end_index = 23
        line = "\tpublic class Searcher\n"
        linesbefore = ["namespace CsSearch\n", "{\n"]
        linesafter = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"]
        searchresult = SearchResult(pattern=pattern, file=sf,
            linenum=linenum, match_start_index=match_start_index,
            match_end_index=match_end_index, line=line, lines_before=linesbefore,
            lines_after=linesafter)
        expectedoutput = """================================================================================
%s: %d: [%d:%d]
--------------------------------------------------------------------------------
   8 | namespace CsSearch
   9 | {
> 10 | \tpublic class Searcher
  11 | \t{
  12 | \t\tprivate readonly FileTypes _fileTypes;
""" % (os.path.join(self.cssearch_path, filename), linenum, match_start_index, match_end_index)
        output = formatter.format(searchresult)
        self.assertEqual(expectedoutput, output)


if __name__ == '__main__':
    unittest.main()
