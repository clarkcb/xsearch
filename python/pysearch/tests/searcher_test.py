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

from pysearch import FileType, Searcher, SearchFile, SearchSettings, SHAREDPATH


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
# is_search_dir tests
################################################################################
    def test_is_search_dir_no_patterns(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        dir = 'plsearch'
        self.assertTrue(searcher.is_search_dir(dir))

    def test_is_search_dir_matches_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('plsearch', 'in_dir_patterns')
        searcher = Searcher(settings)
        dir = 'plsearch'
        self.assertTrue(searcher.is_search_dir(dir))

    def test_is_search_dir_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('plsearch', 'in_dir_patterns')
        searcher = Searcher(settings)
        dir = 'pysearch'
        self.assertFalse(searcher.is_search_dir(dir))

    def test_is_search_dir_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('pysearch', 'out_dir_patterns')
        searcher = Searcher(settings)
        dir = 'pysearch'
        self.assertFalse(searcher.is_search_dir(dir))

    def test_is_search_dir_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('pysearch', 'out_dir_patterns')
        searcher = Searcher(settings)
        dir = 'plsearch'
        self.assertTrue(searcher.is_search_dir(dir))

    def test_is_search_dir_single_dot(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        dir = '.'
        self.assertTrue(searcher.is_search_dir(dir))

    def test_is_search_dir_double_dot(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        dir = '..'
        self.assertTrue(searcher.is_search_dir(dir))

    def test_is_search_dir_hidden_dir(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        dir = '.git'
        self.assertFalse(searcher.is_search_dir(dir))

    def test_is_search_dir_hidden_dir_include_hidden(self):
        settings = self.get_settings()
        settings.exclude_hidden = False
        searcher = Searcher(settings)
        dir = '.git'
        self.assertTrue(searcher.is_search_dir(dir))

################################################################################
# is_search_file tests
################################################################################
    def test_is_search_file_matches_by_default(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        f = SearchFile(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertTrue(searcher.is_search_file(f))

    def test_is_search_file_matches_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('pm', 'in_extensions')
        searcher = Searcher(settings)
        f = SearchFile(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertTrue(searcher.is_search_file(f))

    def test_is_search_file_no_match_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('pl', 'in_extensions')
        searcher = Searcher(settings)
        f = SearchFile(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertFalse(searcher.is_search_file(f))

    def test_is_search_file_matches_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('pm', 'out_extensions')
        searcher = Searcher(settings)
        f = SearchFile(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertFalse(searcher.is_search_file(f))

    def test_is_search_file_no_match_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('py', 'out_extensions')
        searcher = Searcher(settings)
        f = SearchFile(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertTrue(searcher.is_search_file(f))

    def test_is_search_file_matches_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Search', 'in_file_patterns')
        searcher = Searcher(settings)
        f = SearchFile(path='.', file_name='Searcher.pm', file_type=FileType.CODE)
        self.assertTrue(searcher.is_search_file(f))

    def test_is_search_file_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Search', 'in_file_patterns')
        searcher = Searcher(settings)
        f = SearchFile(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertFalse(searcher.is_search_file(f))

    def test_is_search_file_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Search', 'out_file_patterns')
        searcher = Searcher(settings)
        f = SearchFile(path='.', file_name='Searcher.pm', file_type=FileType.CODE)
        self.assertFalse(searcher.is_search_file(f))

    def test_is_search_file_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Search', 'out_file_patterns')
        searcher = Searcher(settings)
        f = SearchFile(path='.', file_name='FileUtil.pm', file_type=FileType.CODE)
        self.assertTrue(searcher.is_search_file(f))

################################################################################
# is__archive_search_file tests
################################################################################
    def test_is_archive_search_file_matches_by_default(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_matches_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('zip', 'in_archive_extensions')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_no_match_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('gz', 'in_archive_extensions')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertFalse(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_matches_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('zip', 'out_archive_extensions')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertFalse(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_no_match_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('gz', 'out_archive_extensions')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_matches_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('arch', 'in_archive_file_patterns')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('archives', 'in_archive_file_patterns')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertFalse(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('arch', 'out_archive_file_patterns')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertFalse(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('archives', 'out_archive_file_patterns')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.is_archive_search_file(f))

################################################################################
# filter_file tests
################################################################################
    def test_filter_file_matches_by_default(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        f = SearchFile(path='', file_name='FileUtil.pm', file_type=FileType.TEXT)
        self.assertTrue(searcher.filter_file(f))

    def test_filter_file_is_search_file(self):
        settings = self.get_settings()
        settings.add_exts('pm', 'in_extensions')
        searcher = Searcher(settings)
        f = SearchFile(path='', file_name='FileUtil.pm', file_type=FileType.TEXT)
        self.assertTrue(searcher.filter_file(f))

    def test_filter_file_not_is_search_file(self):
        settings = self.get_settings()
        settings.add_exts('pl', 'in_extensions')
        searcher = Searcher(settings)
        f = SearchFile(path='', file_name='FileUtil.pm', file_type=FileType.TEXT)
        self.assertFalse(searcher.filter_file(f))

    def test_filter_file_is_hidden_file(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        f = SearchFile(path='', file_name='.gitignore', file_type=FileType.UNKNOWN)
        self.assertFalse(searcher.filter_file(f))

    def test_filter_file_hidden_includehidden(self):
        settings = self.get_settings()
        settings.exclude_hidden = False
        searcher = Searcher(settings)
        f = SearchFile(path='', file_name='.gitignore', file_type=FileType.UNKNOWN)
        self.assertTrue(searcher.filter_file(f))

    def test_filter_file_archive_no_search_archives(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        f = SearchFile(path='', file_name='archive.zip', file_type=FileType.ARCHIVE)
        self.assertFalse(searcher.filter_file(f))

    def test_filter_file_archive_search_archives(self):
        settings = self.get_settings()
        settings.search_archives = 1
        searcher = Searcher(settings)
        f = SearchFile(path='', file_name='archive.zip', file_type=FileType.ARCHIVE)
        self.assertTrue(searcher.filter_file(f))

    def test_filter_file_archive_archives_only(self):
        settings = self.get_settings()
        settings.archives_only = True
        settings.search_archives = True
        searcher = Searcher(settings)
        f = SearchFile(path='', file_name='archive.zip', file_type=FileType.ARCHIVE)
        self.assertTrue(searcher.filter_file(f))

    def test_filter_file_non_archive_archives_only(self):
        settings = self.get_settings()
        settings.archives_only = True
        settings.search_archives = True
        searcher = Searcher(settings)
        f = SearchFile(path='', file_name='FileUtil.pm', file_type=FileType.TEXT)
        self.assertFalse(searcher.filter_file(f))

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
