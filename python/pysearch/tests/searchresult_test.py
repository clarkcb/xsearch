# -*- coding: utf-8 -*-
################################################################################
#
# fileutil_test.py
#
# class FileUtilTest: testing of FileUtil
#
################################################################################
import os
import unittest

from pysearch import FileType, SearchFile, SearchResult, XSEARCHPATH


class SearchResultTest(unittest.TestCase):

    cssearch_path = "%s/csharp/CsSearch/CsSearch" % XSEARCHPATH

    def test_singleline_searchresult(self):
        pattern = "Search"
        filename = 'Searcher.cs'
        sf = SearchFile(path=self.cssearch_path, filename=filename, filetype=FileType.TEXT)
        linenum = 10
        match_start_index = 15
        match_end_index = 23
        line = "\tpublic class Searcher\n"
        linesbefore = []
        linesafter = []
        searchresult = SearchResult(pattern=pattern, file=sf,
            linenum=linenum, match_start_index=match_start_index,
            match_end_index=match_end_index, line=line, lines_before=linesbefore,
            lines_after=linesafter)
        expectedoutput = "%s: %d: [%d:%d]: %s" % (os.path.join(self.cssearch_path, filename), linenum,
            match_start_index, match_end_index, line.strip())
        self.assertEqual(expectedoutput, str(searchresult))

    def test_binaryfile_searchresult(self):
        pattern = "Search"
        filename = 'Searcher.exe'
        sf = SearchFile(path=self.cssearch_path, filename=filename, filetype=FileType.BINARY)
        linenum = 0
        match_start_index = 0
        match_end_index = 0
        line = ''
        linesbefore = []
        linesafter = []
        searchresult = SearchResult(pattern=pattern, file=sf,
            linenum=linenum, match_start_index=match_start_index,
            match_end_index=match_end_index, line=line, lines_before=linesbefore,
            lines_after=linesafter)
        expectedoutput = "%s matches at [%d:%d]" % (os.path.join(self.cssearch_path, filename), match_start_index,
            match_end_index)
        self.assertEqual(expectedoutput, str(searchresult))

    def test_multiline_searchresult(self):
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
        self.assertEqual(expectedoutput, str(searchresult))

if __name__ == '__main__':
    unittest.main()
