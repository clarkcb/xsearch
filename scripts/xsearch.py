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
    'C++': 'cpp',
    'cpp': 'cpp',
    'C#': 'csharp',
    'cs': 'csharp',
    'csharp': 'csharp',
    'dart': 'dart',
    'elixir': 'elixir',
    'ex': 'elixir',
    'F#': 'fsharp',
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

default_startpath = XSEARCHPATH


def lines_for_diff(lines: list[str],
                   skip_blanks: bool = False,
                   sort_lines: bool = False,
                   case_insensitive_cmp: bool = False,
                   normalize_field_names: bool = False) -> list[str]:
    """Return lines modified according to different settings"""
    diff_lines = lines[:]
    if skip_blanks:
        diff_lines = [line for line in diff_lines if line.strip() != '']
    if sort_lines:
        diff_lines = list(sorted(diff_lines))
    if case_insensitive_cmp:
        diff_lines = [line.upper() for line in diff_lines]
    if normalize_field_names:
        diff_lines = [
            line.replace('_', '').replace('-', '')
            for line in diff_lines
        ]
    return diff_lines


def non_matching_lens(xsearch_output: dict[str, list[str]],
                      skip_blanks: bool = True) -> list[tuple[str, str]]:
    """Examines xsearch_output (a dict of {xsearch_name : [lines]})
       and returns a list of tuples of non-matching xsearch pairs
       ([(xsearch_name_1, xsearch_name_2)]
    """
    non_matching = []
    xs = sorted(xsearch_output.keys())
    while xs:
        x = xs.pop(0)
        x_lines = lines_for_diff(xsearch_output[x], skip_blanks=skip_blanks)
        x_len = len(x_lines)
        for y in xs:
            y_lines = lines_for_diff(xsearch_output[y], skip_blanks=skip_blanks)
            y_len = len(y_lines)
            if x_len != y_len:
                x_and_y = tuple(sorted([x, y]))
                if x_and_y not in non_matching:
                    non_matching.append(x_and_y)
    return non_matching


def non_matching_outputs(xsearch_output: dict[str, list[str]],
                         sort_lines: bool = True,
                         skip_blanks: bool = True,
                         case_insensitive_cmp: bool = False,
                         normalize_field_names: bool = False) -> list[tuple[str, str]]:
    """Examines xsearch_output (a dict of {xsearch_name : [lines]})
       and returns a list of tuples of non-matching xsearch pairs
      ([(xsearch_name_1, xsearch_name_2)]
    """
    non_matching = []
    xs = sorted(xsearch_output.keys())
    while xs:
        x = xs.pop(0)
        x_lines = lines_for_diff(xsearch_output[x],
                                 skip_blanks=skip_blanks,
                                 sort_lines=sort_lines,
                                 case_insensitive_cmp=case_insensitive_cmp,
                                 normalize_field_names=normalize_field_names)
        for y in xs:
            y_lines = lines_for_diff(xsearch_output[y],
                                     skip_blanks=skip_blanks,
                                     sort_lines=sort_lines,
                                     case_insensitive_cmp=case_insensitive_cmp,
                                     normalize_field_names=normalize_field_names)
            if x_lines != y_lines:
                # print("\n{}:\n\"{}\"".format(x, x_output))
                # print("\n{}:\n\"{}\"".format(y, y_output))
                x_and_y = tuple(sorted([x, y]))
                if x_and_y not in non_matching:
                    non_matching.append(x_and_y)
    return non_matching
