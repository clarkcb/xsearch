# -*- coding: utf-8 -*-
################################################################################
#
# searchoptions_test.py
#
# class SearchOptionsTest: testing of SearchOptions class
#
################################################################################
import re
import sys
import unittest

sys.path.insert(0, '..')

from pysearch.searchoptions import SearchOptions

class SearchOptionsTest(unittest.TestCase):
    @classmethod
    def setUpClass(self):
        self.searchoptions = SearchOptions()

    def test_no_args(self):
        # test the props
        settings = self.searchoptions.search_settings_from_args([])
        self.assertFalse(settings.archivesonly)
        self.assertFalse(settings.debug)
        self.assertFalse(settings.dotiming)
        self.assertFalse(settings.firstmatch)
        self.assertTrue(settings.excludehidden)
        self.assertEqual(settings.linesafter, 0)
        self.assertEqual(settings.linesbefore, 0)
        self.assertFalse(settings.listdirs)
        self.assertFalse(settings.listfiles)
        self.assertFalse(settings.listlines)
        self.assertEqual(settings.maxlinelength, 150)
        self.assertFalse(settings.multilinesearch)
        self.assertTrue(settings.printresults)
        self.assertFalse(settings.printusage)
        self.assertFalse(settings.printversion)
        self.assertTrue(settings.recursive)
        self.assertFalse(settings.searcharchives)
        self.assertFalse(settings.uniquelines)
        self.assertFalse(settings.verbose)
        # test the extensino and pattern sets
        self.assertFalse(settings.in_archiveextensions)
        self.assertFalse(settings.in_archivefilepatterns)
        self.assertFalse(settings.in_dirpatterns)
        self.assertFalse(settings.in_filepatterns)
        self.assertFalse(settings.in_linesafterpatterns)
        self.assertFalse(settings.in_linesbeforepatterns)
        self.assertFalse(settings.linesaftertopatterns)
        self.assertFalse(settings.linesafteruntilpatterns)
        self.assertFalse(settings.out_archiveextensions)
        self.assertFalse(settings.out_archivefilepatterns)
        self.assertFalse(settings.out_dirpatterns)
        self.assertFalse(settings.out_filepatterns)
        self.assertFalse(settings.out_linesafterpatterns)
        self.assertFalse(settings.out_linesbeforepatterns)
        self.assertFalse(settings.searchpatterns)

    def test_valid_args(self):
        args = ['-x', 'py,rb', '-s', 'Search', '.']
        settings = self.searchoptions.search_settings_from_args(args)
        self.assertEqual(settings.startpath, '.')
        for x in set(['py', 'rb']):
            self.assertIn(x, settings.in_extensions)
        self.assertEquals(list(settings.searchpatterns)[0].pattern, 'Search')

    def test_archivesonly_arg(self):
        args = ['--archivesonly']
        settings = self.searchoptions.search_settings_from_args(args)
        self.assertTrue(settings.archivesonly)
        self.assertTrue(settings.searcharchives)

    def test_debug_arg(self):
        args = ['--debug']
        settings = self.searchoptions.search_settings_from_args(args)
        self.assertTrue(settings.debug)
        self.assertTrue(settings.verbose)

    def test_with_invalid_arg(self):
        args = ['-x', 'py,rb', '-s', 'Search', '.', '-Q']
        with self.assertRaises(Exception) as cm:
            settings = self.searchoptions.search_settings_from_args(args)
        self.assertEqual(cm.exception.message, 'Unknown option: Q')

if __name__ == '__main__':
    unittest.main()
