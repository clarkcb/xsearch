# -*- coding: utf-8 -*-
###############################################################################
#
# searchfile.py
#
# class SearchFile: encapsulates a file to search
#
###############################################################################
from io import StringIO
import os
from typing import List

from .filetypes import FileType


class SearchFile(object):
    """encapsulates a search file"""
    CONTAINER_SEPARATOR = '!'

    __slots__ = ['containers', 'path', 'filename', 'filetype']

    def __init__(self, containers: List[str] = None, path: str = '', filename: str = '',
                 filetype: FileType = FileType.UNKNOWN):
        self.containers = containers if containers else []
        self.path = path
        self.filename = filename
        self.filetype = filetype

    @property
    def relativepath(self):
        return os.path.join(self.path, self.filename)

    def __str__(self):
        sio = StringIO()
        if self.containers:
            sio.write(self.CONTAINER_SEPARATOR.join(self.containers))
            sio.write(self.CONTAINER_SEPARATOR)
        sio.write(self.relativepath)
        # sio.write(' (%s)' % self.filetype)
        return sio.getvalue()
