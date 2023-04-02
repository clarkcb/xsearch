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
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pyfind import FileType, FileResult
from pysearch import Searcher, SearchSettings, SHAREDPATH


class SearcherTest(unittest.TestCase):

    def get_settings(self):
        settings = SearchSettings()
        settings.add_paths('.')
        settings.add_patterns('Searcher', 'search_patterns')
        settings.debug = True
        return settings

    def get_test_file(self):
        return os.path.join(SHAREDPATH, 'testFiles/testFile2.txt')

################################################################################
# is_search_file tests - TODO: remove these after archive find implemented
################################################################################
    def test_is_search_file_matches_by_default(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        fr = FileResult(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertTrue(searcher.finder.is_matching_file(fr))

    def test_is_search_file_matches_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('pm', 'in_extensions')
        searcher = Searcher(settings)
        fr = FileResult(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertTrue(searcher.finder.is_matching_file(fr))

    def test_is_search_file_no_match_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('pl', 'in_extensions')
        searcher = Searcher(settings)
        fr = FileResult(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertFalse(searcher.finder.is_matching_file(fr))

    def test_is_search_file_matches_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('pm', 'out_extensions')
        searcher = Searcher(settings)
        fr = FileResult(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertFalse(searcher.finder.is_matching_file(fr))

    def test_is_search_file_no_match_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('py', 'out_extensions')
        searcher = Searcher(settings)
        fr = FileResult(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertTrue(searcher.finder.is_matching_file(fr))

    def test_is_search_file_matches_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Search', 'in_file_patterns')
        searcher = Searcher(settings)
        fr = FileResult(path='.', file_name='Searcher.pm', file_type=FileType.CODE)
        self.assertTrue(searcher.finder.is_matching_file(fr))

    def test_is_search_file_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Search', 'in_file_patterns')
        searcher = Searcher(settings)
        fr = FileResult(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertFalse(searcher.finder.is_matching_file(fr))

    def test_is_search_file_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Search', 'out_file_patterns')
        searcher = Searcher(settings)
        fr = FileResult(path='.', file_name='Searcher.pm', file_type=FileType.CODE)
        self.assertFalse(searcher.finder.is_matching_file(fr))

    def test_is_search_file_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Search', 'out_file_patterns')
        searcher = Searcher(settings)
        fr = FileResult(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertTrue(searcher.finder.is_matching_file(fr))

################################################################################
# is__archive_search_file tests - TODO: remove...
################################################################################
    def test_is_archive_search_file_matches_by_default(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.finder.is_matching_archive_file(f))

    def test_is_archive_search_file_matches_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('zip', 'in_archive_extensions')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.finder.is_matching_archive_file(f))

    def test_is_archive_search_file_no_match_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('gz', 'in_archive_extensions')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertFalse(searcher.finder.is_matching_archive_file(f))

    def test_is_archive_search_file_matches_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('zip', 'out_archive_extensions')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertFalse(searcher.finder.is_matching_archive_file(f))

    def test_is_archive_search_file_no_match_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('gz', 'out_archive_extensions')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.finder.is_matching_archive_file(f))

    def test_is_archive_search_file_matches_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('arch', 'in_archive_file_patterns')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.finder.is_matching_archive_file(f))

    def test_is_archive_search_file_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('archives', 'in_archive_file_patterns')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertFalse(searcher.finder.is_matching_archive_file(f))

    def test_is_archive_search_file_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('arch', 'out_archive_file_patterns')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertFalse(searcher.finder.is_matching_archive_file(f))

    def test_is_archive_search_file_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('archives', 'out_archive_file_patterns')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.finder.is_matching_archive_file(f))

# ################################################################################
# # filter_file tests
# ################################################################################
#     def test_filter_file_matches_by_default(self):
#         settings = self.get_settings()
#         searcher = Searcher(settings)
#         f = FileResult(path='', filename='FileUtil.pm', filetype=FileType.TEXT)
#         self.assertTrue(searcher.filter_file(f))
#
#     def test_filter_file_is_search_file(self):
#         settings = self.get_settings()
#         settings.add_exts('pm', 'in_extensions')
#         searcher = Searcher(settings)
#         f = FileResult(path='', filename='FileUtil.pm', filetype=FileType.TEXT)
#         self.assertTrue(searcher.filter_file(f))
#
#     def test_filter_file_not_is_search_file(self):
#         settings = self.get_settings()
#         settings.add_exts('pl', 'in_extensions')
#         searcher = Searcher(settings)
#         f = FileResult(path='', filename='FileUtil.pm', filetype=FileType.TEXT)
#         self.assertFalse(searcher.filter_file(f))
#
#     def test_filter_file_is_hidden_file(self):
#         settings = self.get_settings()
#         searcher = Searcher(settings)
#         f = FileResult(path='', filename='.gitignore', filetype=FileType.UNKNOWN)
#         self.assertFalse(searcher.filter_file(f))
#
#     def test_filter_file_hidden_includehidden(self):
#         settings = self.get_settings()
#         settings.excludehidden = False
#         searcher = Searcher(settings)
#         f = FileResult(path='', filename='.gitignore', filetype=FileType.UNKNOWN)
#         self.assertTrue(searcher.filter_file(f))
#
#     def test_filter_file_archive_no_searcharchives(self):
#         settings = self.get_settings()
#         searcher = Searcher(settings)
#         f = FileResult(path='', filename='archive.zip', filetype=FileType.ARCHIVE)
#         self.assertFalse(searcher.filter_file(f))
#
#     def test_filter_file_archive_searcharchives(self):
#         settings = self.get_settings()
#         settings.searcharchives = 1
#         searcher = Searcher(settings)
#         f = FileResult(path='', filename='archive.zip', filetype=FileType.ARCHIVE)
#         self.assertTrue(searcher.filter_file(f))
#
#     def test_filter_file_archive_archivesonly(self):
#         settings = self.get_settings()
#         settings.archivesonly = True
#         settings.searcharchives = True
#         searcher = Searcher(settings)
#         f = FileResult(path='', filename='archive.zip', filetype=FileType.ARCHIVE)
#         self.assertTrue(searcher.filter_file(f))
#
#     def test_filter_file_nonarchive_archivesonly(self):
#         settings = self.get_settings()
#         settings.archivesonly = True
#         settings.searcharchives = True
#         searcher = Searcher(settings)
#         f = FileResult(path='', filename='FileUtil.pm', filetype=FileType.TEXT)
#         self.assertFalse(searcher.filter_file(f))

################################################################################
# search_lines tests
################################################################################
    def test_search_lines(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        testfile = self.get_test_file()
        results = []
        try:
            fo = open(testfile, 'r')
            results = searcher.search_line_iterator(fo)
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

################################################################################
# search_multi_line_string tests
################################################################################
    def test_search_multi_line_string(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        testfile = self.get_test_file()
        results = []
        try:
            fo = open(testfile, 'r')
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
