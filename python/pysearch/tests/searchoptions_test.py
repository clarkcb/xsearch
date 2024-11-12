# -*- coding: utf-8 -*-
"""
################################################################################
#
# searchoptions_test.py
#
# class SearchOptionsTest: testing of SearchOptions class
#
################################################################################
"""
import os
from pathlib import Path
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pysearch import SearchException, SearchOptions, SearchSettings


class SearchOptionsTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.search_options = SearchOptions()

    def test_no_args(self):
        # test the props
        settings = self.search_options.search_settings_from_args([])
        self.assertFalse(settings.archives_only)
        self.assertFalse(settings.debug)
        self.assertFalse(settings.first_match)
        self.assertFalse(settings.include_hidden)
        self.assertEqual(settings.lines_after, 0)
        self.assertEqual(settings.lines_before, 0)
        self.assertFalse(settings.print_dirs)
        self.assertFalse(settings.print_files)
        self.assertFalse(settings.print_lines)
        self.assertEqual(settings.max_line_length, 150)
        self.assertFalse(settings.multi_line_search)
        self.assertTrue(settings.print_results)
        self.assertFalse(settings.print_usage)
        self.assertFalse(settings.print_version)
        self.assertTrue(settings.recursive)
        self.assertFalse(settings.search_archives)
        self.assertFalse(settings.unique_lines)
        self.assertFalse(settings.verbose)
        # test the extension and pattern sets
        self.assertFalse(settings.in_archive_extensions)
        self.assertFalse(settings.in_archive_file_patterns)
        self.assertFalse(settings.in_dir_patterns)
        self.assertFalse(settings.in_file_patterns)
        self.assertFalse(settings.in_lines_after_patterns)
        self.assertFalse(settings.in_lines_before_patterns)
        self.assertFalse(settings.lines_after_to_patterns)
        self.assertFalse(settings.lines_after_until_patterns)
        self.assertFalse(settings.out_archive_extensions)
        self.assertFalse(settings.out_archive_file_patterns)
        self.assertFalse(settings.out_dir_patterns)
        self.assertFalse(settings.out_file_patterns)
        self.assertFalse(settings.out_lines_after_patterns)
        self.assertFalse(settings.out_lines_before_patterns)
        self.assertFalse(settings.search_patterns)

    def test_valid_args(self):
        args = ['-x', 'py,rb', '-s', 'Search', '.']
        settings = self.search_options.search_settings_from_args(args)
        self.assertEqual(1, len(settings.paths))
        for x in {'py', 'rb'}:
            self.assertIn(x, settings.in_extensions)
        self.assertEqual('Search', list(settings.search_patterns)[0].pattern)

    def test_archives_only_arg(self):
        args = ['--archivesonly']
        settings = self.search_options.search_settings_from_args(args)
        self.assertTrue(settings.archives_only)
        self.assertTrue(settings.search_archives)

    def test_debug_arg(self):
        args = ['--debug']
        settings = self.search_options.search_settings_from_args(args)
        self.assertTrue(settings.debug)
        self.assertTrue(settings.verbose)

    def test_missing_arg(self):
        args = ['-x', 'py,rb', '-s', 'Search', '.', '-D']
        with self.assertRaises(SearchException) as cm:
            settings = self.search_options.search_settings_from_args(args)
        self.assertEqual(str(cm.exception), 'Missing value for option D')

    def test_invalid_arg(self):
        args = ['-x', 'py,rb', '-s', 'Search', '.', '-Q']
        with self.assertRaises(SearchException) as cm:
            settings = self.search_options.search_settings_from_args(args)
        self.assertEqual(str(cm.exception), 'Invalid option: Q')

    def test_settings_from_json(self):
        settings = SearchSettings()
        json = '''{
  "path": "~/src/xsearch/",
  "in-ext": ["js","ts"],
  "out-dirpattern": ["node_module", "dist"],
  "out-filepattern": ["temp"],
  "searchpattern": "Searcher",
  "linesbefore": 2,
  "linesafter": 2,
  "debug": true,
  "allmatches": false,
  "includehidden": true
}'''
        self.search_options.settings_from_json(json, settings)
        self.assertEqual(1, len(settings.paths))
        self.assertIn(Path('~/src/xsearch/'), settings.paths)
        for x in {'js', 'ts'}:
            self.assertIn(x, settings.in_extensions)
        self.assertTrue(any([p.pattern == 'Searcher' for p in settings.search_patterns]))
        self.assertTrue(any([p.pattern == 'node_module' for p in settings.out_dir_patterns]))
        self.assertTrue(any([p.pattern == 'dist' for p in settings.out_dir_patterns]))
        self.assertTrue(any([p.pattern == 'temp' for p in settings.out_file_patterns]))
        self.assertEqual(settings.lines_before, 2)
        self.assertEqual(settings.lines_after, 2)
        self.assertTrue(settings.debug)
        self.assertTrue(settings.verbose)
        self.assertTrue(settings.first_match)
        self.assertTrue(settings.include_hidden)


if __name__ == '__main__':
    unittest.main()
