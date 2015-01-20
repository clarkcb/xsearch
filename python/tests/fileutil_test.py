# -*- coding: utf-8 -*-
################################################################################
#
# fileutil_test.py
#
# class FileUtilTest: testing of FileUtil
#
################################################################################
import sys
import unittest

sys.path.insert(0, '~/src/git/xsearch/python')

from pysearch.fileutil import FileUtil

class FileUtilTest(unittest.TestCase):
    def test_get_extension_has_txt_extension(self):
        filename = 'filename.txt'
        self.assertEqual(FileUtil.get_extension(filename), 'txt')

    def test_get_extension_missing_extension(self):
        filename = 'filename.'
        self.assertEqual(FileUtil.get_extension(filename), '')

    def test_get_extension_no_extension(self):
        filename = 'filename'
        self.assertEqual(FileUtil.get_extension(filename), '')

    def test_get_extension_hidden_txt_file(self):
        filename = '.hidden.txt'
        self.assertEqual(FileUtil.get_extension(filename), 'txt')

    def test_get_extension_hidden_file_missing_extension(self):
        filename = '.hidden.'
        self.assertEqual(FileUtil.get_extension(filename), '')

    def test_get_extension_hidden_file_no_extension(self):
        filename = '.hidden'
        self.assertEqual(FileUtil.get_extension(filename), '')

if __name__ == '__main__':
    unittest.main()
