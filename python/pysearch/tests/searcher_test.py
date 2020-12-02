# -*- coding: utf-8 -*-
################################################################################
#
# searcher_test.py
#
# Searcher testing
#
################################################################################
import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from pysearch import FileType, Searcher, SearchFile, SearchSettings, SHAREDPATH


class SearcherTest(unittest.TestCase):

    def get_settings(self):
        settings = SearchSettings()
        settings.startpath = '.'
        settings.add_patterns('Searcher', 'searchpatterns')
        settings.debug = True
        return settings

    def get_test_file(self):
        return '%s/testFiles/testFile2.txt' % SHAREDPATH

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
        settings.add_patterns('plsearch', 'in_dirpatterns')
        searcher = Searcher(settings)
        dir = 'plsearch'
        self.assertTrue(searcher.is_search_dir(dir))

    def test_is_search_dir_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('plsearch', 'in_dirpatterns')
        searcher = Searcher(settings)
        dir = 'pysearch'
        self.assertFalse(searcher.is_search_dir(dir))

    def test_is_search_dir_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('pysearch', 'out_dirpatterns')
        searcher = Searcher(settings)
        dir = 'pysearch'
        self.assertFalse(searcher.is_search_dir(dir))

    def test_is_search_dir_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('pysearch', 'out_dirpatterns')
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
        settings.excludehidden = False
        searcher = Searcher(settings)
        dir = '.git'
        self.assertTrue(searcher.is_search_dir(dir))

################################################################################
# is_search_file tests
################################################################################
    def test_is_search_file_matches_by_default(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        f = SearchFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
        self.assertTrue(searcher.is_search_file(f))

    def test_is_search_file_matches_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('pm', 'in_extensions')
        searcher = Searcher(settings)
        f = SearchFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
        self.assertTrue(searcher.is_search_file(f))

    def test_is_search_file_no_match_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('pl', 'in_extensions')
        searcher = Searcher(settings)
        f = SearchFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
        self.assertFalse(searcher.is_search_file(f))

    def test_is_search_file_matches_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('pm', 'out_extensions')
        searcher = Searcher(settings)
        f = SearchFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
        self.assertFalse(searcher.is_search_file(f))

    def test_is_search_file_no_match_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('py', 'out_extensions')
        searcher = Searcher(settings)
        f = SearchFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
        self.assertTrue(searcher.is_search_file(f))

    def test_is_search_file_matches_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Search', 'in_filepatterns')
        searcher = Searcher(settings)
        f = SearchFile(path='.', filename='Searcher.pm', filetype=FileType.CODE)
        self.assertTrue(searcher.is_search_file(f))

    def test_is_search_file_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Search', 'in_filepatterns')
        searcher = Searcher(settings)
        f = SearchFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
        self.assertFalse(searcher.is_search_file(f))

    def test_is_search_file_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Search', 'out_filepatterns')
        searcher = Searcher(settings)
        f = SearchFile(path='.', filename='Searcher.pm', filetype=FileType.CODE)
        self.assertFalse(searcher.is_search_file(f))

    def test_is_search_file_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Search', 'out_filepatterns')
        searcher = Searcher(settings)
        f = SearchFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
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
        settings.add_exts('zip', 'in_archiveextensions')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_no_match_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('gz', 'in_archiveextensions')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertFalse(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_matches_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('zip', 'out_archiveextensions')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertFalse(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_no_match_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('gz', 'out_archiveextensions')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_matches_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('arch', 'in_archivefilepatterns')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('archives', 'in_archivefilepatterns')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertFalse(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('arch', 'out_archivefilepatterns')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertFalse(searcher.is_archive_search_file(f))

    def test_is_archive_search_file_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('archives', 'out_archivefilepatterns')
        searcher = Searcher(settings)
        f = 'archive.zip'
        self.assertTrue(searcher.is_archive_search_file(f))

################################################################################
# filter_file tests
################################################################################
    def test_filter_file_matches_by_default(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        f = SearchFile(path='', filename='FileUtil.pm', filetype=FileType.TEXT)
        self.assertTrue(searcher.filter_file(f))

    def test_filter_file_is_search_file(self):
        settings = self.get_settings()
        settings.add_exts('pm', 'in_extensions')
        searcher = Searcher(settings)
        f = SearchFile(path='', filename='FileUtil.pm', filetype=FileType.TEXT)
        self.assertTrue(searcher.filter_file(f))

    def test_filter_file_not_is_search_file(self):
        settings = self.get_settings()
        settings.add_exts('pl', 'in_extensions')
        searcher = Searcher(settings)
        f = SearchFile(path='', filename='FileUtil.pm', filetype=FileType.TEXT)
        self.assertFalse(searcher.filter_file(f))

    def test_filter_file_is_hidden_file(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        f = SearchFile(path='', filename='.gitignore', filetype=FileType.UNKNOWN)
        self.assertFalse(searcher.filter_file(f))

    def test_filter_file_hidden_includehidden(self):
        settings = self.get_settings()
        settings.excludehidden = False
        searcher = Searcher(settings)
        f = SearchFile(path='', filename='.gitignore', filetype=FileType.UNKNOWN)
        self.assertTrue(searcher.filter_file(f))

    def test_filter_file_archive_no_searcharchives(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        f = SearchFile(path='', filename='archive.zip', filetype=FileType.ARCHIVE)
        self.assertFalse(searcher.filter_file(f))

    def test_filter_file_archive_searcharchives(self):
        settings = self.get_settings()
        settings.searcharchives = 1
        searcher = Searcher(settings)
        f = SearchFile(path='', filename='archive.zip', filetype=FileType.ARCHIVE)
        self.assertTrue(searcher.filter_file(f))

    def test_filter_file_archive_archivesonly(self):
        settings = self.get_settings()
        settings.archivesonly = True
        settings.searcharchives = True
        searcher = Searcher(settings)
        f = SearchFile(path='', filename='archive.zip', filetype=FileType.ARCHIVE)
        self.assertTrue(searcher.filter_file(f))

    def test_filter_file_nonarchive_archivesonly(self):
        settings = self.get_settings()
        settings.archivesonly = True
        settings.searcharchives = True
        searcher = Searcher(settings)
        f = SearchFile(path='', filename='FileUtil.pm', filetype=FileType.TEXT)
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

        firstResult = results[0]
        self.assertEqual(firstResult.linenum, 29)
        self.assertEqual(firstResult.match_start_index, 3)
        self.assertEqual(firstResult.match_end_index, 11)

        secondResult = results[1]
        self.assertEqual(secondResult.linenum, 35)
        self.assertEqual(secondResult.match_start_index, 24)
        self.assertEqual(secondResult.match_end_index, 32)

################################################################################
# search_multiline_string tests
################################################################################
    def test_search_multiline_string(self):
        settings = self.get_settings()
        searcher = Searcher(settings)
        testfile = self.get_test_file()
        results = []
        try:
            fo = open(testfile, 'r')
            contents = fo.read()
            results = searcher.search_multiline_string(contents)
            fo.close()
        except IOError as e:
            print(('IOError: {0!s}'.format(e)))
        self.assertEqual(len(results), 2)

        firstResult = results[0]
        self.assertEqual(firstResult.linenum, 29)
        self.assertEqual(firstResult.match_start_index, 3)
        self.assertEqual(firstResult.match_end_index, 11)

        secondResult = results[1]
        self.assertEqual(secondResult.linenum, 35)
        self.assertEqual(secondResult.match_start_index, 24)
        self.assertEqual(secondResult.match_end_index, 32)


if __name__ == '__main__':
    unittest.main()
