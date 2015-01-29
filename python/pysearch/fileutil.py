# -*- coding: utf-8 -*-
################################################################################
#
# fileutil.py
#
# class FileUtil: provides utility functions for getting file extension and
#                 determining file type
#
################################################################################
import os

class FileUtil(object):
    """a file helper class"""

    DOT_DIRS = ('.', '..')

    @staticmethod
    def get_extension(filename):
        """Returns the extension for a given filename, if any, else empty 
           string"""
        ext = ''
        if os.path.basename(filename).rfind('.') > 0:
            ext = filename.split('.')[-1]
        return ext.lower()

    @staticmethod
    def is_hidden(filename):
        """Returns true if file is hidden else false"""
        f = os.path.basename(filename)
        if len(f) > 1 and f.startswith('.') and not f in FileUtil.DOT_DIRS:
            return True
        return False
