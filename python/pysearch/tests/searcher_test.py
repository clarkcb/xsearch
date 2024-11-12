# -*- coding: utf-8 -*-
"""
################################################################################
#
# searcher_test.py
#
# Searcher testing
#
################################################################################
"""
import os
from pathlib import Path
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pyfind import FileType, FileResult
from pysearch import Searcher, SearchSettings, SHAREDPATH


def get_settings():
    settings = SearchSettings()
    settings.add_paths('.')
    settings.add_patterns('Searcher', 'search_patterns')
    settings.debug = True
    return settings


class SearcherTest(unittest.TestCase):
    ############################################################################
    # search_lines tests
    ############################################################################
    def test_search_lines(self):
        settings = get_settings()
        searcher = Searcher(settings)
        fr = FileResult(path=Path(SHAREDPATH, 'testFiles','testFile2.txt'),
                        file_type=FileType.TEXT)
        results = []
        try:
            fo = open(fr.relative_path, 'r')
            results = searcher.search_line_iterator(fr, fo)
            fo.close()
        except IOError as e:
            print(('IOError: {0!s}'.format(e)))

        self.assertEqual(len(results), 2)

        first_result = results[0]
        self.assertEqual(first_result.line_num, 29)
        self.assertEqual(first_result.match_start_index, 3)
        self.assertEqual(first_result.match_end_index, 11)

        second_result = results[1]
        self.assertEqual(second_result.line_num, 35)
        self.assertEqual(second_result.match_start_index, 24)
        self.assertEqual(second_result.match_end_index, 32)

    ############################################################################
    # search_multi_line_string tests
    ############################################################################
    def test_search_multi_line_string(self):
        settings = get_settings()
        searcher = Searcher(settings)
        fr = FileResult(path=Path(SHAREDPATH, 'testFiles','testFile2.txt'),
                        file_type=FileType.TEXT)
        results = []
        try:
            fo = open(fr.relative_path, 'r')
            contents = fo.read()
            results = searcher.search_multi_line_string(contents)
            fo.close()
        except IOError as e:
            print(('IOError: {0!s}'.format(e)))

        self.assertEqual(len(results), 2)

        first_result = results[0]
        self.assertEqual(first_result.line_num, 29)
        self.assertEqual(first_result.match_start_index, 3)
        self.assertEqual(first_result.match_end_index, 11)

        second_result = results[1]
        self.assertEqual(second_result.line_num, 35)
        self.assertEqual(second_result.match_start_index, 24)
        self.assertEqual(second_result.match_end_index, 32)


if __name__ == '__main__':
    unittest.main()
