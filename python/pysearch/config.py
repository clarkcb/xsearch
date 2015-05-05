# -*- coding: utf-8 -*-
################################################################################
#
# config.py
#
# Configuration values
#
################################################################################
import os
import platform

HOME_NAME = 'HOME'
if platform.system() == 'Windows':
    HOME_NAME = 'USERPROFILE'

HOME = os.environ[HOME_NAME]
XSEARCHPATH = '%s/src/git/xsearch' % HOME
SHAREDPATH = '%s/shared' % XSEARCHPATH
FILETYPESPATH = '%s/filetypes.xml' % SHAREDPATH
SEARCHOPTIONSPATH = '%s/searchoptions.xml' % SHAREDPATH
