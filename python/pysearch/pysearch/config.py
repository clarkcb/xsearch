# -*- coding: utf-8 -*-
"""
###############################################################################
#
# config.py
#
# Configuration values
#
###############################################################################
"""
import os

cwd = os.path.dirname(os.path.realpath(__file__))
data_path = os.path.join(cwd, 'data')

XSEARCHPATH = os.getenv('XSEARCH_PATH')
if not XSEARCHPATH:
    HOME = os.getenv('HOME')
    XSEARCHPATH = os.path.join(HOME, 'src/xsearch')
SHAREDPATH = os.path.join(XSEARCHPATH, 'shared')
SEARCHOPTIONSPATH = os.path.join(data_path, 'searchoptions.json')
