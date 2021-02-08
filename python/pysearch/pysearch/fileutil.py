# -*- coding: utf-8 -*-
###############################################################################
#
# fileutil.py
#
# class FileUtil: provides utility functions for getting file extension and
#                 determining file type
#
###############################################################################
import os


class FileUtil(object):
    """a file helper class"""

    DOT_DIRS = frozenset(['.', '..'])

    @staticmethod
    def get_extension(filename: str) -> str:
        """Returns the extension for a given filename, if any, else empty
           string"""
        ext = ''
        if os.path.basename(filename).rfind('.') > 0:
            ext = filename.split('.')[-1]
        return ext.lower()

    @staticmethod
    def is_dot_dir(filepath: str) -> bool:
        """Returns true if filepath is dot dir (. or ..)"""
        if not filepath:
            return False
        return filepath in FileUtil.DOT_DIRS or \
            (filepath.endswith('/') and filepath[:-1] in FileUtil.DOT_DIRS)

    @staticmethod
    def is_hidden(filepath: str) -> bool:
        """Returns true if filepath is hidden else false"""
        if FileUtil.is_dot_dir(filepath):
            return False
        dot_elems = [
            e for e in filepath.split(os.path.sep)
            if e.startswith('.') and not FileUtil.is_dot_dir(e)
        ]
        return len(dot_elems) > 0
