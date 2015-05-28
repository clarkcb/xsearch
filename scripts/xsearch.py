#!/usr/bin/env python
# -*- coding: utf-8 -*-
################################################################################
#
# xsearch.py
#
# Shared domain classes, properties, functions for xsearch scripts
#
################################################################################
from collections import namedtuple
import os
import platform
import re

########################################
# Classes
########################################
Scenario = namedtuple('Scenario', ['name', 'args', 'replace_xsearch_name'], verbose=False)
RunResult = namedtuple('RunResult', ['scenario', 'run', 'time_dict'], verbose=False)


########################################
# Configuration
########################################
xsearch_dict = {
    'clojure': 'cljsearch',
    'csharp':  'cssearch',
    'fsharp':  'fssearch',
    'go':      'gosearch',
    'haskell': 'hssearch',
    'java':    'javasearch',
    'node':    'nodesearch',
    'perl':    'plsearch.pl',
    'php':     'phpsearch.php',
    'python':  'pysearch.py',
    'ruby':    'rbsearch.rb',
    'scala':   'scalasearch',
    'swift':   'swiftsearch',
}
all_xsearch_names = sorted(xsearch_dict.values())

xsearch_name_regex = re.compile(r'\b(%s)(\.exe)?\b' % '|'.join(all_xsearch_names), re.I | re.S)

default_runs = 10

HOME_NAME = 'USERPROFILE' if platform.system() == 'Windows' else 'HOME'
HOME = os.environ[HOME_NAME]
XSEARCHPATH = os.path.join(HOME, 'src', 'xsearch')

default_startpath = XSEARCHPATH

def nonmatching_lens(xsearch_output):
    """Examines xsearch_output (a dict of xsearch_name : lines)
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
    """Examines xsearch_output (a dict of xsearch_name : lines)
       and returns a dict of xsearch instances with non-matching
       output ({xsearch_name: [non_matching_xsearch_names]})
    """
    nonmatching = {}
    xs = sorted(xsearch_output.keys())
    while xs:
        x = xs.pop(0)
        for y in xs:
            x_output = ''.join(xsearch_output[x])
            y_output = ''.join(xsearch_output[y])
            if x_output != y_output:
                nonmatching.setdefault(x, []).append(y)
                nonmatching.setdefault(y, []).append(x)
    return nonmatching
