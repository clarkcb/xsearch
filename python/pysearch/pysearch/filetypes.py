# -*- coding: utf-8 -*-
###############################################################################
#
# filetypes.py
#
# class FileTypes
#
###############################################################################
from enum import Enum
import json
import xml.dom.minidom as minidom

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

    __slots__ = ['_filetypes']

    def __init__(self):
        self._filetypes = {}
        self._populate_filetypes_from_json()

    def get_filetype(self, filename: str) -> FileType:
        if self.is_text_file(filename):
            return FileType.TEXT
        if self.is_binary_file(filename):
            return FileType.BINARY
        if self.is_archive_file(filename):
            return FileType.ARCHIVE
        if self.is_code_file(filename):
            return FileType.CODE
        if self.is_xml_file(filename):
            return FileType.XML
        return FileType.UNKNOWN

    def is_archive_file(self, f: str) -> bool:
        """Return true if file is of a (known) archive file type"""
        return FileUtil.get_extension(f) in self._filetypes['archive']

    def is_binary_file(self, f: str) -> bool:
        """Return true if file is of a (known) searchable binary file type"""
        return FileUtil.get_extension(f) in self._filetypes['binary']

    def is_code_file(self, f: str) -> bool:
        """Return true if file is of a (known) code file type"""
        return FileUtil.get_extension(f) in self._filetypes['code']

    def is_searchable_file(self, f: str) -> bool:
        """Return true if file is of a (known) searchable type"""
        return FileUtil.get_extension(f) in self._filetypes['searchable']

    def is_text_file(self, f: str) -> bool:
        """Return true if file is of a (known) text file type"""
        return FileUtil.get_extension(f) in self._filetypes['text']

    def is_xml_file(self, f: str) -> bool:
        """Return true if file is of a (known) xml file type"""
        return FileUtil.get_extension(f) in self._filetypes['xml']

    def _populate_filetypes_from_json(self):
        with open(FILETYPESPATH, mode='r') as f:
            filetypes_dict = json.load(f)
        for filetype_obj in filetypes_dict['filetypes']:
            typename = filetype_obj['type']
            exts = set(filetype_obj['extensions'])
            self._filetypes[typename] = exts
        self._filetypes['text'].update(self._filetypes['code'],
                                       self._filetypes['xml'])
        self._filetypes['searchable'] = \
            self._filetypes['binary'].union(self._filetypes['archive'],
                                           self._filetypes['text'])

    def _populate_filetypes_from_xml(self):
        filetypedom = minidom.parse(FILETYPESPATH)
        filetypenodes = filetypedom.getElementsByTagName('filetype')
        for filetypenode in filetypenodes:
            name = filetypenode.getAttribute('name')
            extnode = filetypenode.getElementsByTagName('extensions')[0]
            exts = set(get_text(extnode.childNodes).split())
            self._filetypes[name] = exts
        self._filetypes['text'].update(self._filetypes['code'],
                                       self._filetypes['xml'])
        self._filetypes['searchable'] = \
            self._filetypes['binary'].union(self._filetypes['archive'],
                                           self._filetypes['text'])
