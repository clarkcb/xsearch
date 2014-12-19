################################################################################
#
# searchfile.py
#
# class SearchFile: encapsulates a file to search
#
################################################################################
from cStringIO import StringIO
import os

from fileutil import FileType, FileUtil

class SearchFile:
    """encapsulates a search file"""
    CONTAINER_SEPARATOR = '!'

    def __init__(self, **kargs):
        self.containers = []
        self.path = ''
        self.filename = ''
        self.filetype = FileType.Unknown
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
        return sio.getvalue()

