# -*- coding: utf-8 -*-
"""
################################################################################
#
# fileutil_test.py
#
# class FileUtilTest: testing of FileUtil
#
################################################################################
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pysearch import FileUtil


class FileUtilTest(unittest.TestCase):
    ################################################################################
    # get_extension tests
    ################################################################################
    def test_get_extension_has_txt_extension(self):
        file_name = 'filename.txt'
        self.assertEqual(FileUtil.get_extension(file_name), 'txt')

    def test_get_extension_missing_extension(self):
        file_name = 'filename.'
        self.assertEqual(FileUtil.get_extension(file_name), '')

    def test_get_extension_no_extension(self):
        file_name = 'filename'
        self.assertEqual(FileUtil.get_extension(file_name), '')

    def test_get_extension_hidden_txt_file(self):
        file_name = '.hidden.txt'
        self.assertEqual(FileUtil.get_extension(file_name), 'txt')

    def test_get_extension_hidden_file_missing_extension(self):
        file_name = '.hidden.'
        self.assertEqual(FileUtil.get_extension(file_name), '')

    def test_get_extension_hidden_file_no_extension(self):
        file_name = '.hidden'
        self.assertEqual(FileUtil.get_extension(file_name), '')

    ################################################################################
    # is_dot_dir tests
    ################################################################################
    def test_is_dot_dir_single_dot(self):
        file_name = '.'
        self.assertTrue(FileUtil.is_dot_dir(file_name))

    def test_is_dot_dir_double_dot(self):
        file_name = '..'
        self.assertTrue(FileUtil.is_dot_dir(file_name))

    def test_is_dot_dir_non_dot_dir(self):
        file_name = '.git'
        self.assertFalse(FileUtil.is_dot_dir(file_name))

    ################################################################################
    # is_hidden tests
    ################################################################################
    def test_is_hidden_hidden_file(self):
        file_name = '.filename.txt'
        self.assertTrue(FileUtil.is_hidden(file_name))

    def test_is_hidden_not_hidden_file(self):
        file_name = 'filename.txt'
        self.assertFalse(FileUtil.is_hidden(file_name))

    def test_is_hidden_single_dot(self):
        file_name = '.'
        self.assertFalse(FileUtil.is_hidden(file_name))

    def test_is_hidden_double_dot(self):
        file_name = '..'
        self.assertFalse(FileUtil.is_hidden(file_name))


if __name__ == '__main__':
    unittest.main()
