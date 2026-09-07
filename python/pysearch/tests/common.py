# -*- coding: utf-8 -*-
"""
################################################################################
#
# common.py
#
# Common testing utilities
#
################################################################################
"""
import os


def get_xsearch_path() -> str:
    _home = os.getenv('HOME', '')
    _default_xsearch_path = os.path.join(_home, 'src', 'xsearch')
    _xsearch_path = os.getenv('XSEARCH_PATH', _default_xsearch_path)
    return _xsearch_path
