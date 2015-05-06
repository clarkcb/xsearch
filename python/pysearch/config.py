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

HOME_NAME = 'USERPROFILE' if platform.system() == 'Windows' else 'HOME'
HOME = os.environ[HOME_NAME]
XSEARCHPATH = os.path.join(HOME, 'src', 'git', 'xsearch')
SHAREDPATH = os.path.join(XSEARCHPATH, 'shared')
FILETYPESPATH = os.path.join(SHAREDPATH, 'filetypes.xml')
SEARCHOPTIONSPATH = os.path.join(SHAREDPATH, 'searchoptions.xml')
