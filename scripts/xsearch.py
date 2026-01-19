# -*- coding: utf-8 -*-
################################################################################
#
# xsearch.py
#
# Shared domain classes, properties, functions for xsearch scripts
#
################################################################################
import os
import platform
import re

########################################
# Configuration
########################################
lang_alias_dict = {
    'bash': 'bash',
    'C': 'c',
    'c': 'c',
    'clj': 'clojure',
    'clojure': 'clojure',
    'c++': 'cpp',
    'cpp': 'cpp',
    'c#': 'csharp',
    'cs': 'csharp',
    'csharp': 'csharp',
    'dart': 'dart',
    'elixir': 'elixir',
    'ex': 'elixir',
    'f#': 'fsharp',
    'fs': 'fsharp',
    'fsharp': 'fsharp',
    'go': 'go',
    'groovy': 'groovy',
    'haskell': 'haskell',
    'hs': 'haskell',
    'java': 'java',
    'javascript': 'javascript',
    'js': 'javascript',
    'kotlin': 'kotlin',
    'kt': 'kotlin',
    'objc': 'objc',
    'ocaml': 'ocaml',
    'ml': 'ocaml',
    'perl': 'perl',
    'pl': 'perl',
    'php': 'php',
    'powershell': 'powershell',
    'ps1': 'powershell',
    'pwsh': 'powershell',
    'py': 'python',
    'python': 'python',
    'rb': 'ruby',
    'ruby': 'ruby',
    'rs': 'rust',
    'rust': 'rust',
    'scala': 'scala',
    'swift': 'swift',
    'ts': 'typescript',
    'typescript': 'typescript'
}
xsearch_dict = {
    # 'bash':       'bashsearch',
    # 'c':          'csearch',
    'clojure':    'cljsearch',
    'cpp':        'cppsearch',
    'csharp':     'cssearch',
    'dart':       'dartsearch',
    'elixir':     'exsearch',
    'fsharp':     'fssearch',
    'go':         'gosearch',
    # 'groovy':     'groovysearch',
    'haskell':    'hssearch',
    'java':       'javasearch',
    'javascript': 'jssearch',
    'kotlin':     'ktsearch',
    'objc':       'objcsearch',
    # 'ocaml':      'mlsearch',
    'perl':       'plsearch',
    'php':        'phpsearch',
    'powershell': 'ps1search',
    'python':     'pysearch',
    'ruby':       'rbsearch',
    'rust':       'rssearch',
    'scala':      'scalasearch',
    'swift':      'swiftsearch',
    'typescript': 'tssearch',
}
win_supported = [ 'csharp', 'fsharp', 'go', 'haskell', 'javascript', 'perl', 'python', 'ruby']
all_xsearch_names = sorted(list(set(xsearch_dict.values())))
all_langs = sorted(list(set(xsearch_dict.keys())))
HOME_NAME = 'HOME'
if platform.system() == 'Windows':
    HOME_NAME = 'USERPROFILE'
    all_xsearch_names = sorted([xsearch_dict[l] for l in win_supported])

xsearch_name_regex = re.compile(r'\b({})(\.exe)?\b'.format('|'.join(all_xsearch_names)), re.I | re.S)

default_runs = 10

HOME = os.environ[HOME_NAME]

# set XSEARCHPATH, default to $HOME/src/xsearch but override with env var if defined
XSEARCHPATH = os.path.join(HOME, 'src', 'xsearch')
if 'XSEARCH_PATH' in os.environ:
    XSEARCHPATH = os.environ['XSEARCH_PATH']
elif 'XSEARCHPATH' in os.environ:
    XSEARCHPATH = os.environ['XSEARCHPATH']

# set XFINDPATH, default to $HOME/src/xfind but override with env var if defined
XFINDPATH = os.path.join(HOME, 'src', 'xfind')
if 'XFIND_PATH' in os.environ:
    XFINDPATH = os.environ['XFIND_PATH']
elif 'XFINDPATH' in os.environ:
    XFINDPATH = os.environ['XFINDPATH']

default_startpath = XSEARCHPATH
