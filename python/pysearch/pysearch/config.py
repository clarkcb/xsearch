# -*- coding: utf-8 -*-
###############################################################################
#
# config.py
#
# Configuration values
#
###############################################################################
import json
import os

cwd = os.path.dirname(os.path.realpath(__file__))
config_json_path = os.path.join(cwd, '../../../shared/config.json')
config = json.load(open(config_json_path))

XSEARCHPATH = config['xsearchpath']
SHAREDPATH = os.path.join(XSEARCHPATH, 'shared')
FILETYPESPATH = os.path.join(SHAREDPATH, 'filetypes.json')
SEARCHOPTIONSPATH = os.path.join(SHAREDPATH, 'searchoptions.json')
