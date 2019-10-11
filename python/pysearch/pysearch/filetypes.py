# -*- coding: utf-8 -*-
###############################################################################
#
# filetypes.py
#
# class FileTypes
#
###############################################################################
import xml.dom.minidom as minidom

from .common import get_text
from .config import FILETYPESPATH
from .fileutil import FileUtil
from .searchexception import SearchException


class FileType(object):
    """FileType enum"""
    Unknown = 0
    Archive = 1
    Binary  = 2
    Code    = 3
    Text    = 4
    Xml     = 5

    @classmethod
    def from_name(cls, name):
        uname = name.upper()
        if uname == 'TEXT':
            return FileType.Text
        if uname == 'BINARY':
            return FileType.Binary
        if uname == 'ARCHIVE':
            return FileType.Archive
        if uname == 'CODE':
            return FileType.Code
        if uname == 'XML':
            return FileType.Xml
        raise SearchException('Invalid file type: {0!s}\n'.format(name))


class FileTypes(object):
    """a class to provide file type information"""

    def __init__(self, **kargs):
        self.filetypes = {}
        self.__dict__.update(kargs)
        self.__populate_filetypes()

    def get_filetype(self, filename):
        if self.is_text_file(filename):
            return FileType.Text
        if self.is_binary_file(filename):
            return FileType.Binary
        if self.is_archive_file(filename):
            return FileType.Archive
        if self.is_code_file(filename):
            return FileType.Code
        if self.is_xml_file(filename):
            return FileType.Xml
        return FileType.Unknown

    def is_archive_file(self, f):
        """Return true if file is of a (known) archive file type"""
        return FileUtil.get_extension(f) in self.filetypes['archive']

    def is_binary_file(self, f):
        """Return true if file is of a (known) searchable binary file type"""
        return FileUtil.get_extension(f) in self.filetypes['binary']

    def is_code_file(self, f):
        """Return true if file is of a (known) code file type"""
        return FileUtil.get_extension(f) in self.filetypes['code']

    def is_searchable_file(self, f):
        """Return true if file is of a (known) searchable type"""
        return FileUtil.get_extension(f) in self.filetypes['searchable']

    def is_text_file(self, f):
        """Return true if file is of a (known) text file type"""
        return FileUtil.get_extension(f) in self.filetypes['text']

    def is_xml_file(self, f):
        """Return true if file is of a (known) xml file type"""
        return FileUtil.get_extension(f) in self.filetypes['xml']

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
