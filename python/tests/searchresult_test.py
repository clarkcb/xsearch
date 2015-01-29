# -*- coding: utf-8 -*-
################################################################################
#
# fileutil_test.py
#
# class FileUtilTest: testing of FileUtil
#
################################################################################
import sys
import unittest

sys.path.insert(0, '~/src/git/xsearch/python')

from pysearch.searchresult import SearchResult

class SearchResultTest(unittest.TestCase):

    def test_singleline_searchresult(self):
        pattern = "Search"
        filepath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        linenum = 10
        match_start_index = 15
        match_end_index = 23
        line = "\tpublic class Searcher\n"
        linesbefore = []
        linesafter = []
        searchresult = SearchResult(pattern=pattern, filename=filepath,
            linenum=linenum, match_start_index=match_start_index,
            match_end_index=match_end_index, line=line, linesbefore=linesbefore,
            linesafter=linesafter)
        expectedoutput = "%s: %d: [%d:%d]: %s" % (filepath, linenum,
            match_start_index, match_end_index, line.strip())
        self.assertEquals(expectedoutput, str(searchresult))

    def test_binaryfile_searchresult(self):
        pattern = "Search"
        filepath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.exe"
        linenum = 0
        match_start_index = 0
        match_end_index = 0
        line = None
        linesbefore = []
        linesafter = []
        searchresult = SearchResult(pattern=pattern, filename=filepath,
            linenum=linenum, match_start_index=match_start_index,
            match_end_index=match_end_index, line=line, linesbefore=linesbefore,
            linesafter=linesafter)
        expectedoutput = "%s matches" % filepath
        self.assertEquals(expectedoutput, str(searchresult))

    def test_multiline_searchresult(self):
        pattern = "Search"
        filepath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        linenum = 10
        match_start_index = 15
        match_end_index = 23
        line = "\tpublic class Searcher\n"
        linesbefore = ["namespace CsSearch\n", "{\n"]
        linesafter = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"]
        searchresult = SearchResult(pattern=pattern, filename=filepath,
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
""" % (filepath, linenum, match_start_index, match_end_index)
        self.assertEquals(expectedoutput, str(searchresult))

if __name__ == '__main__':
    unittest.main()
