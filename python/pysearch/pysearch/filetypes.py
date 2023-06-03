# -*- coding: utf-8 -*-
"""
###############################################################################
#
# filetypes.py
#
# class FileTypes
#
###############################################################################
"""
import importlib.resources
import json
from enum import Enum

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
            raise SearchException(f'Invalid file type: {name}\n')


class FileTypes(object):
    """a class to provide file type information"""

    TEXT_TYPES = frozenset([FileType.CODE, FileType.TEXT, FileType.XML])

    __slots__ = ['__file_types']

    def __init__(self):
        self.__file_types = {}
        self.__populate_filetypes_from_json()

    def get_file_type(self, file_name: str) -> FileType:
        if self.is_code_file(file_name):
            return FileType.CODE
        if self.is_xml_file(file_name):
            return FileType.XML
        if self.is_text_file(file_name):
            return FileType.TEXT
        if self.is_binary_file(file_name):
            return FileType.BINARY
        if self.is_archive_file(file_name):
            return FileType.ARCHIVE
        return FileType.UNKNOWN

    def is_archive_file(self, f: str) -> bool:
        """Return true if file is of a (known) archive file type"""
        return FileUtil.get_extension(f) in self.__file_types['archive']

    def is_binary_file(self, f: str) -> bool:
        """Return true if file is of a (known) searchable binary file type"""
        return FileUtil.get_extension(f) in self.__file_types['binary']

    def is_code_file(self, f: str) -> bool:
        """Return true if file is of a (known) code file type"""
        return FileUtil.get_extension(f) in self.__file_types['code']

    def is_searchable_file(self, f: str) -> bool:
        """Return true if file is of a (known) searchable type"""
        return FileUtil.get_extension(f) in self.__file_types['searchable']

    def is_text_file(self, f: str) -> bool:
        """Return true if file is of a (known) text file type"""
        return FileUtil.get_extension(f) in self.__file_types['text']

    def is_xml_file(self, f: str) -> bool:
        """Return true if file is of a (known) xml file type"""
        return FileUtil.get_extension(f) in self.__file_types['xml']

    def __populate_filetypes_from_json(self):
        data = importlib.resources.files('pysearch').joinpath('data')
        file_types_json = data.joinpath('filetypes.json').read_text()
        file_types_dict = json.loads(file_types_json)
        for file_type_obj in file_types_dict['filetypes']:
            typename = file_type_obj['type']
            exts = set(file_type_obj['extensions'])
            self.__file_types[typename] = exts
        self.__file_types['text'].update(self.__file_types['code'],
                                         self.__file_types['xml'])
        self.__file_types['searchable'] = \
            self.__file_types['binary'].union(self.__file_types['archive'],
                                              self.__file_types['text'])
