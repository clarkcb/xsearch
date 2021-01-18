# -*- coding: utf-8 -*-
###############################################################################
#
# filetypes.py
#
# class FileTypes
#
###############################################################################
import json
import xml.dom.minidom as minidom
from enum import Enum

from .common import get_text
from .config import FILETYPESPATH
from .fileutil import FileUtil
from .searchexception import SearchException


class FileType(Enum):
    """FileType enum"""
    UNKNOWN = 0
    ARCHIVE = 1
    BINARY = 2
    CODE = 3
    TEXT = 4
    XML = 5

    @classmethod
    def from_name(cls, name):
        uname = name.upper()
        try:
            return FileType[uname]
        except KeyError:
            raise SearchException('Invalid file type: {0!s}\n'.format(name))


class FileTypes(object):
    """a class to provide file type information"""

    TEXT_TYPES = frozenset([FileType.CODE, FileType.TEXT, FileType.XML])

    __slots__ = ['__filetypes']

    def __init__(self):
        self.__filetypes = {}
        self.__populate_filetypes_from_json()

    def get_filetype(self, filename: str) -> FileType:
        if self.is_code_file(filename):
            return FileType.CODE
        if self.is_xml_file(filename):
            return FileType.XML
        if self.is_text_file(filename):
            return FileType.TEXT
        if self.is_binary_file(filename):
            return FileType.BINARY
        if self.is_archive_file(filename):
            return FileType.ARCHIVE
        return FileType.UNKNOWN

    def is_archive_file(self, f: str) -> bool:
        """Return true if file is of a (known) archive file type"""
        return FileUtil.get_extension(f) in self.__filetypes['archive']

    def is_binary_file(self, f: str) -> bool:
        """Return true if file is of a (known) searchable binary file type"""
        return FileUtil.get_extension(f) in self.__filetypes['binary']

    def is_code_file(self, f: str) -> bool:
        """Return true if file is of a (known) code file type"""
        return FileUtil.get_extension(f) in self.__filetypes['code']

    def is_searchable_file(self, f: str) -> bool:
        """Return true if file is of a (known) searchable type"""
        return FileUtil.get_extension(f) in self.__filetypes['searchable']

    def is_text_file(self, f: str) -> bool:
        """Return true if file is of a (known) text file type"""
        return FileUtil.get_extension(f) in self.__filetypes['text']

    def is_xml_file(self, f: str) -> bool:
        """Return true if file is of a (known) xml file type"""
        return FileUtil.get_extension(f) in self.__filetypes['xml']

    def __populate_filetypes_from_json(self):
        with open(FILETYPESPATH, mode='r') as f:
            filetypes_dict = json.load(f)
        for filetype_obj in filetypes_dict['filetypes']:
            typename = filetype_obj['type']
            exts = set(filetype_obj['extensions'])
            self.__filetypes[typename] = exts
        self.__filetypes['text'].update(self.__filetypes['code'],
                                        self.__filetypes['xml'])
        self.__filetypes['searchable'] = \
            self.__filetypes['binary'].union(self.__filetypes['archive'],
                                             self.__filetypes['text'])

    def __populate_filetypes_from_xml(self):
        filetypedom = minidom.parse(FILETYPESPATH)
        filetypenodes = filetypedom.getElementsByTagName('filetype')
        for filetypenode in filetypenodes:
            name = filetypenode.getAttribute('name')
            extnode = filetypenode.getElementsByTagName('extensions')[0]
            exts = set(get_text(extnode.childNodes).split())
            self.__filetypes[name] = exts
        self.__filetypes['text'].update(self.__filetypes['code'],
                                        self.__filetypes['xml'])
        self.__filetypes['searchable'] = \
            self.__filetypes['binary'].union(self.__filetypes['archive'],
                                             self.__filetypes['text'])
