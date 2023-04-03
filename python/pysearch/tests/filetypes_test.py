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

from pysearch import FileType, FileTypes


class FileTypesTest(unittest.TestCase):
    @classmethod
    def setUpClass(self):
        self.filetypes = FileTypes()

    def test_get_filetype_archive_file(self):
        filename = 'archive.zip'
        self.assertEqual(self.filetypes.get_filetype(filename), FileType.ARCHIVE)

    def test_get_filetype_binary_file(self):
        filename = 'binary.exe'
        self.assertEqual(self.filetypes.get_filetype(filename), FileType.BINARY)

    def test_get_filetype_text_file(self):
        filename = 'text.txt'
        self.assertEqual(self.filetypes.get_filetype(filename), FileType.TEXT)

    def test_get_filetype_unknown_file(self):
        filename = 'unknown.xyz'
        self.assertEqual(self.filetypes.get_filetype(filename), FileType.UNKNOWN)

    def test_is_archive_file(self):
        filename = 'archive.tar.bz2'
        self.assertTrue(self.filetypes.is_archive_file(filename))

    def test_is_binary_file(self):
        filename = 'binary.dylib'
        self.assertTrue(self.filetypes.is_binary_file(filename))

    def test_is_code_file(self):
        filename = 'code.py'
        self.assertTrue(self.filetypes.is_code_file(filename))

    def test_is_xml_file(self):
        filename = 'file.xml'
        self.assertTrue(self.filetypes.is_xml_file(filename))

    def test_is_searchable_file(self):
        filename = 'file.xml'
        self.assertTrue(self.filetypes.is_searchable_file(filename))


if __name__ == '__main__':
    unittest.main()
