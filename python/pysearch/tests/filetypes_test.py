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

from pysearch import FileType, FileTypes


class FileTypesTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.file_types = FileTypes()

    def test_get_file_type_archive_file(self):
        file_name = 'archive.zip'
        self.assertEqual(self.file_types.get_file_type(file_name), FileType.ARCHIVE)

    def test_get_file_type_binary_file(self):
        file_name = 'binary.exe'
        self.assertEqual(self.file_types.get_file_type(file_name), FileType.BINARY)

    def test_get_file_type_text_file(self):
        file_name = 'text.txt'
        self.assertEqual(self.file_types.get_file_type(file_name), FileType.TEXT)

    def test_get_file_type_unknown_file(self):
        file_name = 'unknown.xyz'
        self.assertEqual(self.file_types.get_file_type(file_name), FileType.UNKNOWN)

    def test_is_archive_file(self):
        file_name = 'archive.tar.bz2'
        self.assertTrue(self.file_types.is_archive_file(file_name))

    def test_is_binary_file(self):
        file_name = 'binary.dylib'
        self.assertTrue(self.file_types.is_binary_file(file_name))

    def test_is_code_file(self):
        file_name = 'code.py'
        self.assertTrue(self.file_types.is_code_file(file_name))

    def test_is_xml_file(self):
        file_name = 'file.xml'
        self.assertTrue(self.file_types.is_xml_file(file_name))

    def test_is_searchable_file(self):
        file_name = 'file.xml'
        self.assertTrue(self.file_types.is_searchable_file(file_name))


if __name__ == '__main__':
    unittest.main()
