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
xsearch_dict = {
    # 'c':           'csearch',
    'clj':         'cljsearch',
    'clojure':     'cljsearch',
    'cpp':         'cppsearch',
    'cs':          'cssearch',
    'csharp':      'cssearch',
    'dart':        'dartsearch',
    'fs':          'fssearch',
    'fsharp':      'fssearch',
    'go':          'gosearch',
    'haskell':     'hssearch',
    'hs':          'hssearch',
    'java':        'javasearch',
    'javascript':  'jssearch',
    'js':          'jssearch',
    'kotlin':     'ktsearch',
    'kt':         'ktsearch',
    'objc':       'objcsearch',
    # 'ocaml':      'mlsearch',
    # 'ml':         'mlsearch',
    'perl':       'plsearch',
    'pl':         'plsearch',
    'php':        'phpsearch',
    'python':     'pysearch',
    'py':         'pysearch',
    'ruby':       'rbsearch',
    'rb':         'rbsearch',
    'rs':         'rssearch',
    'rust':       'rssearch',
    'scala':      'scalasearch',
    'swift':      'swiftsearch',
    'ts':         'tssearch',
    'typescript': 'tssearch',
}
win_supported = [
    'cs', 'csharp', 'fs', 'fsharp', 'go', 'haskell', 'javascript', 'js',
    'perl', 'pl', 'py', 'python', 'rb', 'ruby'
]
all_xsearch_names = sorted(list(set(xsearch_dict.values())))
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

default_startpath = XSEARCHPATH

def nonmatching_lens(xsearch_output):
    """Examines xsearch_output (a dict of {xsearch_name : [lines]})
       and returns a dict of xsearch instances with non-matching
       output line lengths ({xsearch_name: [non_matching_xsearch_names]})
    """
    nonmatching = {}
    xs = sorted(xsearch_output.keys())
    while xs:
        x = xs.pop(0)
        for y in xs:
            x_len = len(xsearch_output[x])
            y_len = len(xsearch_output[y])
            if x_len != y_len:
                nonmatching.setdefault(x, []).append(y)
                nonmatching.setdefault(y, []).append(x)
    return nonmatching

def nonmatching_outputs(xsearch_output):
    """Examines xsearch_output (a dict of {xsearch_name : [lines]})
       and returns a dict of xsearch instances with non-matching
       output ({xsearch_name: [non_matching_xsearch_names]})
    """
    nonmatching = {}
    xs = sorted(xsearch_output.keys())
    while xs:
        x = xs.pop(0)
        for y in xs:
            x_output = xsearch_output[x]
            y_output = xsearch_output[y]
            if x_output != y_output:
                # print("\n{}:\n\"{}\"".format(x, x_output))
                # print("\n{}:\n\"{}\"".format(y, y_output))
                nonmatching.setdefault(x, []).append(y)
                nonmatching.setdefault(y, []).append(x)
    return nonmatching
