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

sys.path.insert(1, '../pysearch')

from fileutil import FileUtil


class FileUtilTest(unittest.TestCase):
################################################################################
# get_extension tests
################################################################################
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

################################################################################
# is_dot_dir tests
################################################################################
    def test_is_dot_dir_single_dot(self):
        filename = '.'
        self.assertTrue(FileUtil.is_dot_dir(filename))

    def test_is_dot_dir_double_dot(self):
        filename = '..'
        self.assertTrue(FileUtil.is_dot_dir(filename))

    def test_is_dot_dir_non_dot_dir(self):
        filename = '.git'
        self.assertFalse(FileUtil.is_dot_dir(filename))

################################################################################
# is_hidden tests
################################################################################
    def test_is_hidden_hidden_file(self):
        filename = '.filename.txt'
        self.assertTrue(FileUtil.is_hidden(filename))

    def test_is_hidden_not_hidden_file(self):
        filename = 'filename.txt'
        self.assertFalse(FileUtil.is_hidden(filename))

    def test_is_hidden_single_dot(self):
        filename = '.'
        self.assertFalse(FileUtil.is_hidden(filename))

    def test_is_hidden_double_dot(self):
        filename = '..'
        self.assertFalse(FileUtil.is_hidden(filename))


if __name__ == '__main__':
    unittest.main()
