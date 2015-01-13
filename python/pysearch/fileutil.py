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

    @staticmethod
    def get_extension(filename):
        """Returns the extension for a given filename, if any, else empty 
           string"""
        ext = ''
        if os.path.basename(filename).rfind('.') > 0:
            ext = filename.split('.')[-1]
        return ext.lower()
