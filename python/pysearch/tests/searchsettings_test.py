# -*- coding: utf-8 -*-
################################################################################
#
# searchsettings_test.py
#
# class SearchSettingsTest: testing of SearchSettings class
#
################################################################################
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__))[:-6])

from pysearch import SearchSettings


class SearchSettingsTest(unittest.TestCase):
    def setUp(self):
        self.settings = SearchSettings()

    def test_default_settings(self):
        # test the props
        self.assertFalse(self.settings.archivesonly)
        self.assertFalse(self.settings.debug)
        self.assertFalse(self.settings.firstmatch)
        self.assertTrue(self.settings.excludehidden)
        self.assertEqual(0, self.settings.linesafter)
        self.assertEqual(0, self.settings.linesbefore)
        self.assertFalse(self.settings.listdirs)
        self.assertFalse(self.settings.listfiles)
        self.assertFalse(self.settings.listlines)
        self.assertEqual(150, self.settings.maxlinelength)
        self.assertFalse(self.settings.multilinesearch)
        self.assertTrue(self.settings.printresults)
        self.assertFalse(self.settings.printusage)
        self.assertFalse(self.settings.printversion)
        self.assertTrue(self.settings.recursive)
        self.assertFalse(self.settings.searcharchives)
        self.assertFalse(self.settings.uniquelines)
        self.assertFalse(self.settings.verbose)
        # test the extension and pattern sets
        self.assertFalse(self.settings.in_archiveextensions)
        self.assertFalse(self.settings.in_archivefilepatterns)
        self.assertFalse(self.settings.in_dirpatterns)
        self.assertFalse(self.settings.in_filepatterns)
        self.assertFalse(self.settings.in_linesafterpatterns)
        self.assertFalse(self.settings.in_linesbeforepatterns)
        self.assertFalse(self.settings.linesaftertopatterns)
        self.assertFalse(self.settings.linesafteruntilpatterns)
        self.assertFalse(self.settings.out_archiveextensions)
        self.assertFalse(self.settings.out_archivefilepatterns)
        self.assertFalse(self.settings.out_dirpatterns)
        self.assertFalse(self.settings.out_filepatterns)
        self.assertFalse(self.settings.out_linesafterpatterns)
        self.assertFalse(self.settings.out_linesbeforepatterns)
        self.assertFalse(self.settings.searchpatterns)
        self.assertEqual(0, len(self.settings.paths))

    def test_set_properties(self):
        props = {
            'archivesonly': True,
            'debug': True,
            'firstmatch': True,
            'excludehidden': False,
            'linesafter': 5,
            'linesbefore': 5,
            'listdirs': True,
            'listfiles': True,
            'listlines': True,
            'maxlinelength': 155,
            'multilinesearch': True,
            'printresults': False,
            'printusage': True,
            'printversion': True,
            'recursive': False,
            'searcharchives': True,
            'uniquelines': True,
            'verbose': True,
        }
        self.settings.set_properties(props)
        self.assertEqual(True, self.settings.archivesonly)
        self.assertEqual(True, self.settings.debug)
        self.assertEqual(True, self.settings.firstmatch)
        self.assertEqual(False, self.settings.excludehidden)
        self.assertEqual(5, self.settings.linesafter)
        self.assertEqual(5, self.settings.linesbefore)
        self.assertEqual(True, self.settings.listdirs)
        self.assertEqual(True, self.settings.listfiles)
        self.assertEqual(True, self.settings.listlines)
        self.assertEqual(155, self.settings.maxlinelength)
        self.assertEqual(True, self.settings.multilinesearch)
        self.assertEqual(False, self.settings.printresults)
        self.assertEqual(True, self.settings.printusage,)
        self.assertEqual(True, self.settings.printversion)
        self.assertEqual(False, self.settings.recursive)
        self.assertEqual(True, self.settings.searcharchives)
        self.assertEqual(True, self.settings.uniquelines)
        self.assertEqual(True, self.settings.verbose)

    def test_add_single_extension(self):
        self.settings.add_exts('py', 'in_extensions')
        self.assertEqual(1, len(self.settings.in_extensions))
        self.assertIn('py', self.settings.in_extensions)

    def test_add_comma_delimited_extensions(self):
        self.settings.add_exts('py,rb,scala', 'in_extensions')
        self.assertEqual(3, len(self.settings.in_extensions))
        for x in {'py', 'rb', 'scala'}:
            self.assertIn(x, self.settings.in_extensions)

    def test_add_extensions_set(self):
        extensions_set = {'py','rb','scala'}
        self.settings.add_exts(extensions_set, 'in_extensions')
        self.assertEqual(3, len(self.settings.in_extensions))
        for x in extensions_set:
            self.assertIn(x, self.settings.in_extensions)

    def test_add_single_pattern(self):
        p = 'Search'
        self.settings.add_patterns(p, 'searchpatterns')
        self.assertEqual(1, len(self.settings.searchpatterns))
        self.assertEqual(p, list(self.settings.searchpatterns)[0].pattern)

    def test_add_patterns_set(self):
        patterns_set = {'Search', 'Test'}
        self.settings.add_patterns(patterns_set, 'searchpatterns')
        self.assertEqual(len(patterns_set), len(self.settings.searchpatterns))
        for p in self.settings.searchpatterns:
            self.assertIn(p.pattern, patterns_set)


if __name__ == '__main__':
    unittest.main()
