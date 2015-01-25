# -*- coding: utf-8 -*-
################################################################################
#
# filetypes_test.py
#
# class FileTypesTest: testing of FileTypes
#
################################################################################
import sys
import unittest

from pysearch.filetypes import FileType, FileTypes

sys.path.insert(0, '/Users/cary/src/git/xsearch/python')

class FileTypesTest(unittest.TestCase):
    @classmethod
    def setUpClass(self):
        self.filetypes = FileTypes()

    def test_get_filetype_archive_file(self):
        filename = 'archive.zip'
        self.assertEqual(self.filetypes.get_filetype(filename), FileType.Archive)

    def test_get_filetype_binary_file(self):
        filename = 'binary.exe'
        self.assertEqual(self.filetypes.get_filetype(filename), FileType.Binary)

    def test_get_filetype_text_file(self):
        filename = 'text.txt'
        self.assertEqual(self.filetypes.get_filetype(filename), FileType.Text)

    def test_get_filetype_unknown_file(self):
        filename = 'unknown.xyz'
        self.assertEqual(self.filetypes.get_filetype(filename), FileType.Unknown)

if __name__ == '__main__':
    unittest.main()
