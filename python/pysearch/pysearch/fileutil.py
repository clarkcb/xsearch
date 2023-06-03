# -*- coding: utf-8 -*-
"""
###############################################################################
#
# fileutil.py
#
# class FileUtil: provides utility functions for getting file extension and
#                 determining file type
#
###############################################################################
"""
import os


class FileUtil(object):
    """a file helper class"""

    DOT_DIRS = frozenset(['.', '..'])

    @staticmethod
    def get_extension(file_name: str) -> str:
        """Returns the extension for a given file name, if any, else empty
           string"""
        ext = ''
        if os.path.basename(file_name).rfind('.') > 0:
            ext = file_name.rpartition('.')[2]
            if ext == 'Z':
                return ext
        return ext.lower()

    @staticmethod
    def is_dot_dir(file_path: str) -> bool:
        """Returns true if file path is dot dir (. or ..)"""
        if not file_path:
            return False
        return file_path in FileUtil.DOT_DIRS or \
            (file_path.endswith('/') and file_path[:-1] in FileUtil.DOT_DIRS)

    @staticmethod
    def is_hidden(file_path: str) -> bool:
        """Returns true if file path is hidden else false"""
        if FileUtil.is_dot_dir(file_path):
            return False
        dot_elems = [
            e for e in file_path.split(os.path.sep)
            if e.startswith('.') and not FileUtil.is_dot_dir(e)
        ]
        return len(dot_elems) > 0
