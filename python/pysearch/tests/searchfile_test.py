# -*- coding: utf-8 -*-
"""
################################################################################
#
# filetypes_test.py
#
# class FileTypesTest: testing of FileTypes
#
################################################################################
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pysearch import FileType, SearchFile


class SearchFileTest(unittest.TestCase):

    def test_search_file_abs_path(self):
        path = '/Users/cary/src/xsearch/python/pysearch'
        file_name = 'searchfile.py'
        search_file = SearchFile(path=path, file_name=file_name, file_type=FileType.CODE)
        self.assertEqual('/Users/cary/src/xsearch/python/pysearch/searchfile.py', search_file.relative_path)
        self.assertEqual('/Users/cary/src/xsearch/python/pysearch/searchfile.py', str(search_file))

    def test_search_file_rel_path1(self):
        path = '.'
        file_name = 'searchfile.py'
        search_file = SearchFile(path=path, file_name=file_name, file_type=FileType.CODE)
        self.assertEqual('./searchfile.py', search_file.relative_path)
        self.assertEqual('./searchfile.py', str(search_file))

    def test_search_file_rel_path2(self):
        path = './'
        file_name = 'searchfile.py'
        search_file = SearchFile(path=path, file_name=file_name, file_type=FileType.CODE)
        self.assertEqual('./searchfile.py', search_file.relative_path)
        self.assertEqual('./searchfile.py', str(search_file))

    def test_search_file_rel_path3(self):
        path = '..'
        file_name = 'searchfile.py'
        search_file = SearchFile(path=path, file_name=file_name, file_type=FileType.CODE)
        self.assertEqual('../searchfile.py', search_file.relative_path)
        self.assertEqual('../searchfile.py', str(search_file))


if __name__ == '__main__':
    unittest.main()
