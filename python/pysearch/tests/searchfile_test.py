# -*- coding: utf-8 -*-
################################################################################
#
# filetypes_test.py
#
# class FileTypesTest: testing of FileTypes
#
################################################################################
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__))[:-6])

from pysearch import FileType, SearchFile


class SearchFileTest(unittest.TestCase):

    def test_searchfile_abs_path(self):
        path = '/Users/cary/src/xsearch/python/pysearch'
        filename = 'searchfile.py'
        searchfile = SearchFile(path=path, filename=filename, filetype=FileType.CODE)
        self.assertEqual('/Users/cary/src/xsearch/python/pysearch/searchfile.py', searchfile.relativepath)
        self.assertEqual('/Users/cary/src/xsearch/python/pysearch/searchfile.py', str(searchfile))

    def test_searchfile_rel_path1(self):
        path = '.'
        filename = 'searchfile.py'
        searchfile = SearchFile(path=path, filename=filename, filetype=FileType.CODE)
        self.assertEqual('./searchfile.py', searchfile.relativepath)
        self.assertEqual('./searchfile.py', str(searchfile))

    def test_searchfile_rel_path2(self):
        path = './'
        filename = 'searchfile.py'
        searchfile = SearchFile(path=path, filename=filename, filetype=FileType.CODE)
        self.assertEqual('./searchfile.py', searchfile.relativepath)
        self.assertEqual('./searchfile.py', str(searchfile))

    def test_searchfile_rel_path3(self):
        path = '..'
        filename = 'searchfile.py'
        searchfile = SearchFile(path=path, filename=filename, filetype=FileType.CODE)
        self.assertEqual('../searchfile.py', searchfile.relativepath)
        self.assertEqual('../searchfile.py', str(searchfile))


if __name__ == '__main__':
    unittest.main()
