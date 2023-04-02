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
data_path = os.path.join(cwd, 'data')
config_json_path = os.path.join(data_path, 'config.json')
config = json.load(open(config_json_path))

XSEARCHPATH = config['xsearchpath']
SHAREDPATH = os.path.join(XSEARCHPATH, 'shared')
FILETYPESPATH = os.path.join(data_path, 'filetypes.json')
SEARCHOPTIONSPATH = os.path.join(data_path, 'searchoptions.json')
VERSION = config['version']
