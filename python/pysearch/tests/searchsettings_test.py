# -*- coding: utf-8 -*-
"""
################################################################################
#
# searchsettings_test.py
#
# class SearchSettingsTest: testing of SearchSettings class
#
################################################################################
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pysearch import SearchSettings


class SearchSettingsTest(unittest.TestCase):
    def setUp(self):
        self.settings = SearchSettings()

    def test_default_settings(self):
        # test the props
        self.assertFalse(self.settings.archives_only)
        self.assertFalse(self.settings.debug)
        self.assertFalse(self.settings.first_match)
        self.assertFalse(self.settings.include_hidden)
        self.assertEqual(0, self.settings.lines_after)
        self.assertEqual(0, self.settings.lines_before)
        self.assertFalse(self.settings.print_dirs)
        self.assertFalse(self.settings.print_files)
        self.assertFalse(self.settings.print_lines)
        self.assertEqual(150, self.settings.max_line_length)
        self.assertFalse(self.settings.multi_line_search)
        self.assertTrue(self.settings.print_results)
        self.assertFalse(self.settings.print_usage)
        self.assertFalse(self.settings.print_version)
        self.assertTrue(self.settings.recursive)
        self.assertFalse(self.settings.search_archives)
        self.assertFalse(self.settings.unique_lines)
        self.assertFalse(self.settings.verbose)
        # test the extension and pattern sets
        self.assertFalse(self.settings.in_archive_extensions)
        self.assertFalse(self.settings.in_archive_file_patterns)
        self.assertFalse(self.settings.in_dir_patterns)
        self.assertFalse(self.settings.in_file_patterns)
        self.assertFalse(self.settings.in_lines_after_patterns)
        self.assertFalse(self.settings.in_lines_before_patterns)
        self.assertFalse(self.settings.lines_after_to_patterns)
        self.assertFalse(self.settings.lines_after_until_patterns)
        self.assertFalse(self.settings.out_archive_extensions)
        self.assertFalse(self.settings.out_archive_file_patterns)
        self.assertFalse(self.settings.out_dir_patterns)
        self.assertFalse(self.settings.out_file_patterns)
        self.assertFalse(self.settings.out_lines_after_patterns)
        self.assertFalse(self.settings.out_lines_before_patterns)
        self.assertFalse(self.settings.search_patterns)
        self.assertEqual(0, len(self.settings.paths))

    def test_set_properties(self):
        props = {
            'archives_only': True,
            'debug': True,
            'first_match': True,
            'include_hidden': True,
            'lines_after': 5,
            'lines_before': 5,
            'print_dirs': True,
            'print_files': True,
            'print_lines': True,
            'max_line_length': 155,
            'multi_line_search': True,
            'print_results': False,
            'print_usage': True,
            'print_version': True,
            'recursive': False,
            'search_archives': True,
            'unique_lines': True,
            'verbose': True,
        }
        self.settings.set_properties(props)
        self.assertEqual(True, self.settings.archives_only)
        self.assertEqual(True, self.settings.debug)
        self.assertEqual(True, self.settings.first_match)
        self.assertEqual(True, self.settings.include_hidden)
        self.assertEqual(5, self.settings.lines_after)
        self.assertEqual(5, self.settings.lines_before)
        self.assertEqual(True, self.settings.print_dirs)
        self.assertEqual(True, self.settings.print_files)
        self.assertEqual(True, self.settings.print_lines)
        self.assertEqual(155, self.settings.max_line_length)
        self.assertEqual(True, self.settings.multi_line_search)
        self.assertEqual(False, self.settings.print_results)
        self.assertEqual(True, self.settings.print_usage,)
        self.assertEqual(True, self.settings.print_version)
        self.assertEqual(False, self.settings.recursive)
        self.assertEqual(True, self.settings.search_archives)
        self.assertEqual(True, self.settings.unique_lines)
        self.assertEqual(True, self.settings.verbose)

    def test_add_single_extension(self):
        self.settings.add_strs_to_set('py', 'in_extensions')
        self.assertEqual(1, len(self.settings.in_extensions))
        self.assertIn('py', self.settings.in_extensions)

    def test_add_comma_delimited_extensions(self):
        self.settings.add_strs_to_set('py,rb,scala', 'in_extensions')
        self.assertEqual(3, len(self.settings.in_extensions))
        for x in {'py', 'rb', 'scala'}:
            self.assertIn(x, self.settings.in_extensions)

    def test_add_extensions_set(self):
        extensions_set = {'py','rb','scala'}
        self.settings.add_strs_to_set(extensions_set, 'in_extensions')
        self.assertEqual(3, len(self.settings.in_extensions))
        for x in extensions_set:
            self.assertIn(x, self.settings.in_extensions)

    def test_add_single_pattern(self):
        p = 'Search'
        self.settings.add_patterns(p, 'search_patterns')
        self.assertEqual(1, len(self.settings.search_patterns))
        self.assertEqual(p, list(self.settings.search_patterns)[0].pattern)

    def test_add_patterns_set(self):
        patterns_set = {'Search', 'Test'}
        self.settings.add_patterns(patterns_set, 'search_patterns')
        self.assertEqual(len(patterns_set), len(self.settings.search_patterns))
        for p in self.settings.search_patterns:
            self.assertIn(p.pattern, patterns_set)


if __name__ == '__main__':
    unittest.main()
