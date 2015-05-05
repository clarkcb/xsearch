# -*- coding: utf-8 -*-
################################################################################
#
# fileutil.py
#
# class FileUtil: provides utility functions for getting file extension and
#                 determining file type
#
################################################################################
import os
import xml.dom.minidom as minidom

from common import get_text
from config import FILETYPESPATH
from fileutil import FileUtil

class FileType(object):
    """FileType enum"""
    Archive = 1
    Binary  = 2
    Text    = 3
    Unknown = 4

class FileTypes(object):
    """a class to provide file type information"""

    def __init__(self, **kargs):
        self.filetypes = {}
        self.__dict__.update(kargs)
        self.__populate_filetypes()

    def get_filetype(self, filename):
        if self.is_text_file(filename):
            return FileType.Text
        elif self.is_binary_file(filename):
            return FileType.Binary
        elif self.is_archive_file(filename):
            return FileType.Archive
        else:
            return FileType.Unknown

    def is_archive_file(self, f):
        """Return true if file is of a (known) archive file type"""
        return FileUtil.get_extension(f) in self.filetypes['archive']

    def is_binary_file(self, f):
        """Return true if file is of a (known) searchable binary file type"""
        return FileUtil.get_extension(f) in self.filetypes['binary']

    def is_searchable_file(self, f):
        """Return true if file is of a (known) searchable type"""
        return FileUtil.get_extension(f) in self.filetypes['searchable']

    def is_text_file(self, f):
        """Return true if file is of a (known) text file type"""
        return FileUtil.get_extension(f) in self.filetypes['text']

    def __populate_filetypes(self):
        filetypedom = minidom.parse(FILETYPESPATH)
        filetypenodes = filetypedom.getElementsByTagName('filetype')
        for filetypenode in filetypenodes:
            name = filetypenode.getAttribute('name')
            extnode = filetypenode.getElementsByTagName('extensions')[0]
            exts = set(get_text(extnode.childNodes).split())
            self.filetypes[name] = exts
        self.filetypes['text'] = \
            self.filetypes['text'].union(self.filetypes['code'],
                                         self.filetypes['xml'])
        self.filetypes['searchable'] = \
            self.filetypes['binary'].union(self.filetypes['archive'],
                                           self.filetypes['text'])
