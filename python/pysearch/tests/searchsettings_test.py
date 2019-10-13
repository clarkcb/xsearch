# -*- coding: utf-8 -*-
################################################################################
#
# searchsettings_test.py
#
# class SearchSettingsTest: testing of SearchSettings class
#
################################################################################
import unittest

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
        self.assertEqual(self.settings.linesafter, 0)
        self.assertEqual(self.settings.linesbefore, 0)
        self.assertFalse(self.settings.listdirs)
        self.assertFalse(self.settings.listfiles)
        self.assertFalse(self.settings.listlines)
        self.assertEqual(self.settings.maxlinelength, 150)
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
        self.assertEqual(self.settings.archivesonly, True)
        self.assertEqual(self.settings.debug, True)
        self.assertEqual(self.settings.firstmatch, True)
        self.assertEqual(self.settings.excludehidden, False)
        self.assertEqual(self.settings.linesafter, 5)
        self.assertEqual(self.settings.linesbefore, 5)
        self.assertEqual(self.settings.listdirs, True)
        self.assertEqual(self.settings.listfiles, True)
        self.assertEqual(self.settings.listlines, True)
        self.assertEqual(self.settings.maxlinelength, 155)
        self.assertEqual(self.settings.multilinesearch, True)
        self.assertEqual(self.settings.printresults, False)
        self.assertEqual(self.settings.printusage, True)
        self.assertEqual(self.settings.printversion, True)
        self.assertEqual(self.settings.recursive, False)
        self.assertEqual(self.settings.searcharchives, True)
        self.assertEqual(self.settings.uniquelines, True)
        self.assertEqual(self.settings.verbose, True)

    def test_add_single_extension(self):
        self.settings.add_exts('py', 'in_extensions')
        self.assertIn('py', self.settings.in_extensions)

    def test_add_comma_delimited_extensions(self):
        self.settings.add_exts('py,rb,scala', 'in_extensions')
        self.assertEqual(len(self.settings.in_extensions), 3)
        for x in {'py', 'rb', 'scala'}:
            self.assertIn(x, self.settings.in_extensions)

    def test_add_patterns(self):
        p = 'Search'
        self.settings.add_patterns(p, 'searchpatterns')
        self.assertEqual(list(self.settings.searchpatterns)[0].pattern, p)


if __name__ == '__main__':
    unittest.main()
