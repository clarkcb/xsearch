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
from pprint import pprint
import subprocess
import sys
import time

########################################
# Classes
########################################
Scenario = namedtuple('Scenario', ['name', 'args', 'compare_output'], verbose=False)
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
}
all_xsearch_names = sorted(xsearch_dict.values())

default_runs = 10

default_startpath = os.path.expanduser('~/src/git/xsearch/')


def nonmatching_lens(xsearch_output):
    """Examines xsearch_output (a dict of xsearch_name : lines)
       and returns a dict of xsearch instances with non-matching
       output line lengths ({xsearch_name: [non_matching_xsearch_names]})
    """
    nonmatching = {}
    for x in sorted(xsearch_output.keys()):
        for y in sorted([y for y in xsearch_output.keys() if y != x]):
            x_len = len(xsearch_output[x])
            y_len = len(xsearch_output[y])
            if x_len != y_len:
                nonmatching.setdefault(x, []).append(y)
                nonmatching.setdefault(y, []).append(x)
        del(xsearch_output[x])
    return nonmatching

def nonmatching_outputs(xsearch_output):
    """Examines xsearch_output (a dict of xsearch_name : lines)
       and returns a dict of xsearch instances with non-matching
       output ({xsearch_name: [non_matching_xsearch_names]})
    """
    nonmatching = {}
    for x in sorted(xsearch_output.keys()):
        for y in sorted([y for y in xsearch_output.keys() if y != x]):
            x_output = ''.join(sorted(xsearch_output[x]))
            y_output = ''.join(sorted(xsearch_output[y]))
            if x_output != y_output:
                nonmatching.setdefault(x, []).append(y)
                nonmatching.setdefault(y, []).append(x)
        del(xsearch_output[x])
    return nonmatching

