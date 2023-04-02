# -*- coding: utf-8 -*-
"""
###############################################################################
#
# searchfile.py
#
# class SearchFile: encapsulates a file to search
#
###############################################################################
"""
import os
from io import StringIO
from typing import List

from pyfind import FileType


class SearchFile(object):
    """encapsulates a search file"""
    CONTAINER_SEPARATOR = '!'

    __slots__ = ['containers', 'path', 'file_name', 'file_type']

    def __init__(self, containers: List[str] = None, path: str = '', file_name: str = '',
                 file_type: FileType = FileType.UNKNOWN):
        self.containers = containers if containers else []
        self.path = path
        self.file_name = file_name
        self.file_type = file_type

    @property
    def relative_path(self):
        return os.path.join(self.path, self.file_name)

    def __str__(self):
        sio = StringIO()
        if self.containers:
            sio.write(self.CONTAINER_SEPARATOR.join(self.containers))
            sio.write(self.CONTAINER_SEPARATOR)
        sio.write(self.relative_path)
        return sio.getvalue()

    def __lt__(self, other):
        if self.path == other.path:
            return self.file_name < other.file_name
        return self.path < other.path

    def __eq__(self, other):
        return self.path == other.path and self.file_name == other.file_name
