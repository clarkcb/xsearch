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

from .filetypes import FileType


class SearchFile(object):
    """encapsulates a search file"""
    CONTAINER_SEPARATOR = '!'

    def __init__(self, **kargs):
        self.containers = []
        self.path = ''
        self.filename = ''
        self.filetype = FileType.UNKNOWN
        self.__dict__.update(kargs)

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
