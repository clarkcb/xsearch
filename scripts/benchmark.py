#!/usr/bin/env python3
# -*- coding: utf-8 -*-
################################################################################
#
# benchmark.py
#
# A simple benchmarking tool for the various xsearch language versions
#
################################################################################
import argparse
import os
import sys

from xsearch import *

XFIND_SCRIPT_PATH = os.path.join(XFINDPATH, 'scripts')

sys.path.append(XFIND_SCRIPT_PATH)

from benchmarker import *


########################################
# Configuration
########################################
#exts = ','.join('clj cpp cs dart fs go hs java js kt pl php py rb rs scala swift ts'.split())
exts = ','.join('py rb'.split())

startpaths = [os.path.join(XFINDPATH, d) for d in ('python', 'ruby')]
scriptpath = os.path.join(XFINDPATH, 'scripts')
sharedpath = os.path.join(XFINDPATH, 'shared')

default_runs = 10

ignore_dirs = ['build', 'cmake', 'node_modules', 'pycache', 'vendor', 'venv']
ignore_args = [elem for ignore_dir in [['-D', d] for d in ignore_dirs] for elem in ignore_dir]
core_args = ignore_args
ext_args = ['-x', exts]
common_args = core_args + ext_args + startpaths

# Scenarios
basic_scenarios = [
    Scenario('no args', [], replace_exe_name=True),
    Scenario('help', ['-h'], replace_exe_name=True),
]
extension_scenarios = [
    # match extensions
    Scenario('find matching "{}" extensions'.format(exts), common_args),
    Scenario('find not matching "{}" extensions'.format(exts), core_args + ['-X', exts] + startpaths,
             replace_exe_name=False),
]
filename_scenarios = [
    # match filename
    Scenario('find with "find" in filename', common_args + ['-f', 'find']),
    Scenario('find with "find" not in filename', common_args + ['-F', 'find']),
    Scenario('find blank filename (should find nothing)', common_args + ['-f', '^$']),
]
filetype_scenarios = [
    # match filetype
    Scenario('find "audio" filetype', core_args + ['-t', 'audio'] + startpaths),
    Scenario('find not "audio" filetype', core_args + ['-T', 'audio'] + startpaths),
    Scenario('find "binary" filetype', core_args + ['-t', 'binary'] + startpaths),
    Scenario('find not "binary" filetype', core_args + ['-T', 'binary'] + startpaths),
    Scenario('find "code" filetype', core_args + ['-t', 'code'] + startpaths),
    Scenario('find not "code" filetype', core_args + ['-T', 'code'] + startpaths),
    Scenario('find "font" filetype', core_args + ['-t', 'font'] + startpaths),
    Scenario('find not "font" filetype', core_args + ['-T', 'font'] + startpaths),
    Scenario('find "image" filetype', core_args + ['-t', 'image'] + startpaths),
    Scenario('find not "image" filetype', core_args + ['-T', 'image'] + startpaths),
    Scenario('find "text" filetype', core_args + ['-t', 'text'] + startpaths),
    Scenario('find not "text" filetype', core_args + ['-T', 'text'] + startpaths),
    Scenario('find "video" filetype', core_args + ['-t', 'video'] + startpaths),
    Scenario('find not "video" filetype', core_args + ['-T', 'video'] + startpaths),
    Scenario('find "xml" filetype', core_args + ['-t', 'xml'] + startpaths),
    Scenario('find not "xml" filetype', core_args + ['-T', 'xml'] + startpaths),
]
print_scenarios = [
    # print dirs
    Scenario('print matching dirs for "{}" extensions'.format(exts), common_args + ['--printdirs']),
    Scenario('print not matching dirs for "{}" extensions'.format(exts), core_args + ['-X', exts, '--printdirs'] + startpaths,
             replace_exe_name=False),
]
sorting_scenarios = [
    # sorting scenarios
    Scenario('sort shared files by path', ['--sort-by', 'path', sharedpath]),
    Scenario('sort shared files by filename', ['--sort-by', 'name', sharedpath]),
    Scenario('sort scripts files by path case-insensitive', core_args + ['--sort-by', 'path', '--sort-caseinsensitive', scriptpath]),
    Scenario('sort scripts files by filename case-insensitive', core_args + ['--sort-by', 'name', '--sort-caseinsensitive', scriptpath]),
    Scenario('sort shared files by filesize', ['--sort-by', 'size', sharedpath]),
    Scenario('sort shared files by filesize descending', ['--sort-by', 'size', '--sort-descending', sharedpath]),
    Scenario('sort shared files by filetype', ['--sort-by', 'type', sharedpath]),
    Scenario('sort shared files by filetype descending', ['--sort-by', 'type', '--sort-descending', sharedpath]),
    Scenario('sort shared files by lastmod', ['--sort-by', 'lastmod', sharedpath]),
    Scenario('sort shared files by lastmod descending', ['--sort-by', 'lastmod', '--sort-descending', sharedpath]),
]
maxmindepth_scenarios = [
    # filter by maxdepth/mindepth
    Scenario('filter files with depth <= maxdepth', core_args + ['--maxdepth', '3', scriptpath]),
    Scenario('filter files with depth >= mindepth', core_args + ['--mindepth', '2', scriptpath]),
    Scenario('filter files by maxdepth and mindepth', core_args + ['--maxdepth', '3', '--mindepth', '2', scriptpath]),
    Scenario('filter files by invalid range maxdepth and mindepth', ignore_args + ['--maxdepth', '2', '--mindepth', '3', scriptpath], replace_exe_name=True),
]
maxminlastmod_scenarios = [
    # filter by maxlastmod/minlastmod
    Scenario('filter files with lastmod <= maxlastmod', core_args + ['--maxlastmod', '2022-12-30', scriptpath]),
    Scenario('filter files with lastmod >= minlastmod', core_args + ['--minlastmod', '2020-01-01', scriptpath]),
    Scenario('filter files by maxlastmod and minlastmod', core_args + ['--maxlastmod', '2022-12-30', '--minlastmod', '2020-01-01', scriptpath]),
    Scenario('filter files by invalid range maxlastmod and minlastmod', core_args + ['--maxlastmod', '2020-01-01', '--minlastmod', '2022-12-30', scriptpath], replace_exe_name=True),
]
maxminsize_scenarios = [
    # filter by maxsize/minsize
    Scenario('filter files with size < maxsize', core_args + ['--maxsize', '10000', scriptpath]),
    Scenario('filter files with size > minsize', core_args + ['--minsize', '1000', scriptpath]),
    Scenario('filter files by maxsize and minsize', core_args + ['--maxsize', '10000', '--minsize', '5000', scriptpath]),
    Scenario('filter files by invalid range maxsize and minsize', core_args + ['--maxsize', '5000', '--minsize', '10000', scriptpath], replace_exe_name=True),
]
# settingsonly_scenarios = [
#     Scenario('settings-only', core_args + ['-t', 'code'] + ['--settingsonly'], replace_exe_name=True, case_insensitive_cmp=True),
# ]
scenarios = \
    basic_scenarios + \
    extension_scenarios + \
    filename_scenarios + \
    filetype_scenarios + \
    print_scenarios + \
    sorting_scenarios + \
    maxmindepth_scenarios + \
    maxminlastmod_scenarios + \
    maxminsize_scenarios
# scenarios = \
#     maxminlastmod_scenarios + \
#     maxminsize_scenarios

time_keys = {'real', 'sys', 'user', 'total'}


########################################
# Main functions
########################################
def get_parser():
    parser = argparse.ArgumentParser(description='Run xsearch benchmark')
    parser.add_argument('-g', '--groups', action='extend', nargs='*', help='Names of scenario groups to run')
    parser.add_argument('-G', '--skip-groups', action='extend', nargs='*', help='Names of scenario groups to skip')
    parser.add_argument('-s', '--scenarios', action='extend', nargs='*', help='Names of scenarios to run')
    parser.add_argument('-S', '--skip-scenarios', action='extend', nargs='*', help='Names of scenarios to skip')
    parser.add_argument('-l', '--langs', action='extend', nargs='*', help='Languages to include in benchmark')
    parser.add_argument('-L', '--skip-langs', action='extend', nargs='*', help='Languages to exclude from benchmark')
    parser.add_argument('-r', '--runs', type=int, help='Number of runs for each scenario')
    parser.add_argument('-b', '--exit-on-diff', action='store_true', help='Exit on first output difference')
    parser.add_argument('-f', '--scenarios-files', action='extend', nargs='*', help='Scenarios json files')
    parser.add_argument('--debug', action='store_true', help='Print debug output')
    return parser


def fix_arg_list(arg_list: list[str]) -> list[str]:
    if len(arg_list) < 2:
        return arg_list
    if all([len(c) == 1 for c in arg_list]) and any([c not in lang_alias_dict for c in arg_list]):
        return [''.join(arg_list)]
    return arg_list


def main():
    # Defaults
    groups = []
    langs = []
    scenarios = []
    skip_groups = []
    skip_langs = []
    skip_scenarios = []
    runs = default_runs
    debug = False
    exit_on_diff = True
    default_scenarios_files = ['scenarios.json']
    scenarios_files = []

    # skip_groups = ['settings-only']
    # skip_scenarios = ['use invalid settings-file']

    parser = get_parser()
    parsed_args = parser.parse_args(sys.argv[1:])

    if parsed_args.debug:
        debug = True

    if parsed_args.groups:
        groups = fix_arg_list(parsed_args.groups)

    if parsed_args.skip_groups:
        skip_groups = fix_arg_list(parsed_args.skip_groups)

    if parsed_args.scenarios:
        scenarios = fix_arg_list(parsed_args.scenarios)

    if parsed_args.skip_scenarios:
        skip_scenarios = fix_arg_list(parsed_args.skip_scenarios)

    if parsed_args.langs:
        parsed_langs = fix_arg_list(parsed_args.langs)
        alias_langs = []
        for l in [p.lower() for p in parsed_langs]:
            alias_langs.extend(l.split(','))
        for alias_lang in sorted(alias_langs):
            if alias_lang in lang_alias_dict:
                lang = lang_alias_dict[alias_lang]
                langs.append(lang)
            else:
                print(f'Skipping unknown language: {alias_lang}')
        langs = list(sorted(set(langs)))
    else:
        langs = all_langs

    if parsed_args.skip_langs:
        parsed_skip_langs = fix_arg_list(parsed_args.skip_langs)
        alias_skip_langs = []
        for l in [p.lower() for p in parsed_skip_langs]:
            alias_skip_langs.extend(l.split(','))
        for alias_skip_lang in sorted(alias_skip_langs):
            if alias_skip_lang in lang_alias_dict:
                skip_lang = lang_alias_dict[alias_skip_lang]
                skip_langs.append(skip_lang)
                if skip_lang in langs:
                    langs.remove(skip_lang)
            else:
                print(f'Skipping unknown language: {alias_skip_lang}')

    exe_names = [xsearch_dict[l] for l in langs if l in xsearch_dict]

    if parsed_args.runs:
        runs = parsed_args.runs

    if parsed_args.scenarios_files:
        scenarios_files = fix_arg_list(parsed_args.scenarios_files)

    print(f'debug: {debug}')
    print(f'exit_on_diff: {exit_on_diff}')
    print(f'groups: {groups}')
    print(f'langs ({len(langs)}): [{", ".join(langs)}]')
    print(f'exe_names ({len(exe_names)}): [{", ".join(exe_names)}]')
    print(f'runs: {runs}')
    print(f'scenarios: {scenarios}')
    print(f'scenarios_files: {scenarios_files}')
    print(f'skip_groups: {skip_groups}')
    print(f'skip_langs ({len(skip_langs)}): [{", ".join(skip_langs)}]')
    print(f'skip_scenarios: {skip_scenarios}')
    benchmarker = Benchmarker(langs=langs, exe_names=exe_names, runs=runs,
                              group_names=groups, scenario_names=scenarios,
                              skip_groups=skip_groups, skip_scenarios=skip_scenarios,
                              scenarios_files=scenarios_files,
                              exit_on_diff=exit_on_diff,
                              debug=debug)
    benchmarker.run()


if __name__ == '__main__':
    main()
