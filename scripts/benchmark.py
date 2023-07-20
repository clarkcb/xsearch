#!/usr/bin/env python3
# -*- coding: utf-8 -*-
################################################################################
#
# benchmark.py
#
# A simple benchmarking tool for the various xsearch language versions
#
################################################################################
import os
import re
import subprocess
import sys
from collections import namedtuple
from io import StringIO
from typing import Dict, List, Union

from tabulate import tabulate

from xsearch import *

Scenario = namedtuple('Scenario', ['name', 'args', 'replace_xsearch_name'])


########################################
# Configuration
########################################
#exts = ','.join('clj cpp cs dart fs go hs java js kt pl php py rb rs scala swift ts'.split())
exts = ','.join('py rb'.split())

startpaths = [os.path.join(XSEARCHPATH, d) for d in ('python', 'ruby')]
scriptpath = os.path.join(XSEARCHPATH, 'scripts')
sharedpath = os.path.join(XSEARCHPATH, 'shared')

default_runs = 10

ignore_dirs = ['node_modules', 'vendor', 'venv']
ignore_args = [elem for ignore_dir in [['-D', d] for d in ignore_dirs] for elem in ignore_dir]
search_args = ['-s', 'Searcher']
core_args = ignore_args + search_args
ext_args = ['-x', exts]
common_args = core_args + ext_args + startpaths

# Scenarios to add:
#     -x js,ts -s "Searcher" /Users/cary/src/xsearch/typescript
scenarios = [
    Scenario('no args', [], replace_xsearch_name=True),
    Scenario('help', ['-h'], replace_xsearch_name=True),

    Scenario('search lines #1', common_args, replace_xsearch_name=False),
    Scenario('search contents #1', common_args + ['-m'], replace_xsearch_name=False),

    Scenario('search lines #2 - first match', common_args + ['-1'], replace_xsearch_name=False),
    Scenario('search contents #2 - first match', common_args + ['-m', '-1'], replace_xsearch_name=False),

    Scenario('search with "find" in filename', common_args + ['-f', 'find'], replace_xsearch_name=False),
    Scenario('search with "find" not in filename', common_args + ['-F', 'find'], replace_xsearch_name=False),

    # Scenario('find "code" filetype', core_args + ['-t', 'code'] + startpaths, replace_xsearch_name=False),
    # Scenario('find not "code" filetype', core_args + ['-T', 'code'] + startpaths, replace_xsearch_name=False),
    # Scenario('list matching dirs for "{}" extensions'.format(exts), common_args + ['--listdirs'], replace_xsearch_name=False),
    # Scenario('list not matching dirs for "{}" extensions'.format(exts), core_args + ['-X', exts, '--listdirs'] + startpaths,
    #     replace_xsearch_name=False),

    # Scenario('search lines #3', ['-x', 'js,ts', '-s', 'Searcher', os.path.join(XSEARCHPATH, 'typescript')], replace_xsearch_name=False),
    # Scenario('search lines #3 - first match', ['-x', 'js,ts', '-s', 'Searcher', os.path.join(XSEARCHPATH, 'typescript'), '-1'], replace_xsearch_name=False),
]

time_keys = {'real', 'sys', 'user', 'total'}


########################################
# Classes
########################################
class LangResult(object):
    def __init__(self, name: str, real: float, sys: float, user: float, **kwargs):
        self.name = name
        self.real = real
        self.sys = sys
        self.user = user
        self.__dict__.update(kwargs)

    @property
    def total(self):
        return self.real + self.sys + self.user


class RunResult(object):
    def __init__(self, scenario: Scenario, run: int, lang_results: List[LangResult], **kwargs):
        self.scenario = scenario
        self.run = run
        self.lang_results = lang_results
        self.__dict__.update(kwargs)
        self.__ranks_dict = {}
        for t in time_keys:
            ranks = []
            if t in {'real', 'sys', 'user'}:
                for lr in self.lang_results:
                    ranks.append((lr.__dict__[t], lr.name))
            elif t == 'total':
                for lr in self.lang_results:
                    ranks.append((lr.total, lr.name))
            self.__ranks_dict[t] = [r[1] for r in sorted(ranks, key=lambda r: r[0])]

    @property
    def real_ranks(self):
        return self.__ranks_dict['real']

    @property
    def sys_ranks(self):
        return self.__ranks_dict['sys']

    @property
    def user_ranks(self):
        return self.__ranks_dict['user']

    @property
    def total_ranks(self):
        return self.__ranks_dict['total']


class ScenarioResult(object):
    def __init__(self, scenario: Scenario, index: int, run_results: List[RunResult], **kwargs):
        self.scenario = scenario
        self.index = index
        self.run_results = run_results
        self.__dict__.update(kwargs)
        self.__lang_results_dict = {}
        for r in self.run_results:
            for lr in r.lang_results:
                self.__lang_results_dict.setdefault(lr.name, []).append(lr)
        self.langs = set(self.__lang_results_dict.keys())
        self.__lang_totals_dict = {l: {} for l in self.langs}
        for l in self.langs:
            for t in {'real', 'sys', 'user'}:
                self.__lang_totals_dict[l][t] = \
                    sum([lr.__dict__[t] for lr in self.__lang_results_dict[l]])
            self.__lang_totals_dict[l]['total'] = \
                sum([lr.total for lr in self.__lang_results_dict[l]])
        # for each time type, a list of the languages in order from fastest to slowest
        self.__ranks_dict = {}
        for t in time_keys:
            type_totals = sorted([(self.__lang_totals_dict[l][t], l) for l in self.langs], key=lambda t: t[0])
            self.__ranks_dict[t] = [t[1] for t in type_totals]

    @property
    def runs(self) -> int:
        return len(self.run_results)

    def __rank_of_type(self, lang: str, time_type: str) -> int:
        return self.__ranks_dict[time_type].index(lang) + 1

    def total_real(self, lang: str) -> float:
        return self.__lang_totals_dict[lang]['real']

    def avg_real(self, lang: str) -> float:
        tr = self.total_real(lang)
        return tr / self.runs

    def rank_real(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'real')

    def total_sys(self, lang: str) -> float:
        return self.__lang_totals_dict[lang]['sys']

    def avg_sys(self, lang: str) -> float:
        ts = self.total_sys(lang)
        return ts / self.runs

    def rank_sys(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'sys')

    def total_user(self, lang: str) -> float:
        return self.__lang_totals_dict[lang]['user']

    def avg_user(self, lang: str) -> float:
        tu = self.total_user(lang)
        return tu / self.runs

    def rank_user(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'user')

    def total_total(self, lang: str) -> float:
        return self.__lang_totals_dict[lang]['total']

    def avg_total(self, lang: str) -> float:
        tt = self.total_total(lang)
        return tt / self.runs

    def rank_total(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'total')


class ScenarioResults(object):
    def __init__(self, scenario_results: List[ScenarioResult] = None, **kwargs):
        self.scenario_results = []
        if scenario_results:
            self.scenario_results = scenario_results
        self.__dict__.update(kwargs)
        self.langs = set()
        self._update()

    def _update(self):
        if self.scenario_results:
            self.langs = self.scenario_results[0].langs
        self._lang_totals_dict = { l: { t: 0 for t in time_keys } for l in self.langs }
        for sr in self.scenario_results:
            for l in self.langs:
                self._lang_totals_dict[l]['real'] += sr.total_real(l)
                self._lang_totals_dict[l]['sys'] += sr.total_sys(l)
                self._lang_totals_dict[l]['user'] += sr.total_user(l)
                self._lang_totals_dict[l]['total'] += sr.total_total(l)
        # for each time type, a list of the languages in order from fastest to slowest
        self.__ranks_dict = {}
        for t in time_keys:
            type_totals = sorted([(self._lang_totals_dict[l][t], l) for l in self.langs], key=lambda t: t[0])
            self.__ranks_dict[t] = [t[1] for t in type_totals]

    @property
    def runs(self) -> int:
        return sum([sr.runs for sr in self.scenario_results])

    def append(self, scenario_result: ScenarioResult):
        self.scenario_results.append(scenario_result)
        self._update()

    def __len__(self):
        return len(self.scenario_results)

    def __rank_of_type(self, lang: str, time_type: str) -> int:
        return self.__ranks_dict[time_type].index(lang) + 1

    def total_real(self, lang: str) -> float:
        return self._lang_totals_dict[lang]['real']

    def avg_real(self, lang: str) -> float:
        tr = self.total_real(lang)
        return tr / self.runs

    def rank_real(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'real')

    def total_sys(self, lang: str) -> float:
        return self._lang_totals_dict[lang]['sys']

    def avg_sys(self, lang: str) -> float:
        ts = self.total_sys(lang)
        return ts / self.runs

    def rank_sys(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'sys')

    def total_user(self, lang: str) -> float:
        return self._lang_totals_dict[lang]['user']

    def avg_user(self, lang: str) -> float:
        tu = self.total_user(lang)
        return tu / self.runs

    def rank_user(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'user')

    def total_total(self, lang: str) -> float:
        return self._lang_totals_dict[lang]['total']

    def avg_total(self, lang: str) -> float:
        tt = self.total_total(lang)
        return tt / self.runs

    def rank_total(self, lang: str) -> int:
        return self.__rank_of_type(lang, 'total')


########################################
# Benchmarker class
########################################
class Benchmarker(object):
    def __init__(self, **kwargs):
        self.xsearch_names = all_xsearch_names
        self.scenarios = []
        self.runs = default_runs
        self.debug = True
        self.exit_on_diff = True
        self.exit_on_sort_diff = True
        self.diff_outputs = []
        self.__dict__.update(kwargs)

    def __print_data_table(self, title: str, hdr: List[str], data: List[List[Union[float, int]]], col_types: List[type]):
        print('\n{}'.format(title))
        print(tabulate(data, headers=hdr))

    def print_scenario_summary(self, scenario_results: ScenarioResults):
        title = "\nScenario results summary for {} out of {} scenarios with {} out of {} total runs\n".\
            format(len(scenario_results), len(self.scenarios),
                   scenario_results.runs, len(self.scenarios) * self.runs)
        hdr = []
        for i in range(len(scenario_results)):
            hdr.extend(['S{} total'.format(i + 1), 'S{} avg'.format(i + 1), 'S{} rank'.format(i + 1)])
        hdr.extend(['TOTAL', 'AVG', 'RANK'])
        data = []
        col_types = [float, float, int] * (len(scenario_results) + 1)
        for x in self.xsearch_names:
            row = [x]
            for sr in scenario_results.scenario_results:
                xt = sr.total_total(x)
                xta = sr.avg_total(x)
                xtr = sr.rank_total(x)
                row.extend([xt, xta, xtr])
            xt = scenario_results.total_total(x)
            xta = scenario_results.avg_total(x)
            xtr = scenario_results.rank_total(x)
            row.extend([xt, xta, xtr])
            data.append(row)
        self.__print_data_table(title, hdr, data, col_types)

    def print_scenario_results(self, scenario_results: ScenarioResults):
        title = "\nTotal results for {} out of {} scenarios with {} out of {} total runs\n".\
            format(len(scenario_results.scenario_results), len(self.scenarios),
                   scenario_results.runs, len(self.scenarios) * self.runs)
        hdr = ['real', 'avg', 'rank', 'sys', 'avg', 'rank', 'user', 'avg',
               'rank', 'total', 'avg', 'rank']
        data = []
        col_types = [float, float, int] * 4
        for x in self.xsearch_names:
            xr = scenario_results.total_real(x)
            xra = scenario_results.avg_real(x)
            xrr = scenario_results.rank_real(x)

            xs = scenario_results.total_sys(x)
            xsa = scenario_results.avg_sys(x)
            xsr = scenario_results.rank_sys(x)

            xu = scenario_results.total_user(x)
            xua = scenario_results.avg_user(x)
            xur = scenario_results.rank_user(x)

            xt = scenario_results.total_total(x)
            xta = scenario_results.avg_total(x)
            xtr = scenario_results.rank_total(x)
            data.append([x, xr, xra, xrr, xs, xsa, xsr, xu, xua, xur, xt, xta, xtr])
        self.__print_data_table(title, hdr, data, col_types)

    def print_scenario_result(self, scenario_result: ScenarioResult):
        title = '\nTotal results for scenario {} ("{}") with {} runs\n'.\
            format(scenario_result.index, scenario_result.scenario.name, self.runs)
        hdr = ['real', 'avg', 'rank', 'sys', 'avg', 'rank', 'user',
               'avg', 'rank', 'total', 'avg', 'rank']
        data = []
        col_types = [float, float, int] * 4
        for x in self.xsearch_names:
            xr = scenario_result.total_real(x)
            xra = scenario_result.avg_real(x)
            xrr = scenario_result.rank_real(x)

            xs = scenario_result.total_sys(x)
            xsa = scenario_result.avg_sys(x)
            xsr = scenario_result.rank_sys(x)

            xu = scenario_result.total_user(x)
            xua = scenario_result.avg_user(x)
            xur = scenario_result.rank_user(x)

            xt = scenario_result.total_total(x)
            xta = scenario_result.avg_total(x)
            xtr = scenario_result.rank_total(x)
            data.append([x, xr, xra, xrr, xs, xsa, xsr, xu, xua, xur, xt, xta, xtr])
        self.__print_data_table(title, hdr, data, col_types)

    def print_run_result(self, sn: int, result: RunResult):
        title = '\nResults for scenario {} ("{}") run {}\n'.format(sn, result.scenario.name, result.run)
        hdr = ['real', 'rank', 'sys', 'rank', 'user', 'rank', 'total', 'rank']
        data = []
        real_ranks = result.real_ranks
        sys_ranks = result.sys_ranks
        user_ranks = result.user_ranks
        total_ranks = result.total_ranks
        col_types = [float, int] * 4
        for x in self.xsearch_names:
            lang_result = [lr for lr in result.lang_results if lr.name == x][0]
            xr = lang_result.real
            xrr = real_ranks.index(x) + 1
            xs = lang_result.sys
            xsr = sys_ranks.index(x) + 1
            xu = lang_result.user
            xur = user_ranks.index(x) + 1
            xt = lang_result.total
            xtr = total_ranks.index(x) + 1
            data.append([x, xr, xrr, xs, xsr, xu, xur, xt, xtr])
        self.__print_data_table(title, hdr, data, col_types)

    def times_from_lines(self, lines: List[str]) -> Dict[str,float]:
        time_dict = {}
        times = lines[0].split()
        time_name_matches = [re.match(r'^(\d+(:\d+)?\.\d+)(user|system|elapsed)', t) for t in times[:3]]
        if time_name_matches and all(time_name_matches):
            for time_name_match in time_name_matches:
                n = time_name_match.group(3)
                t = time_name_match.group(1)
                # print('name: "{}", time: {}'.format(n, t))
                if n == 'elapsed':
                    colon_idx = t.find(':')
                    # print('colon_idx: {}'.format(colon_idx))
                    time_dict['real'] = float(t[colon_idx+1:])
                else:
                    if n == 'system':
                        n = 'sys'
                    time_dict[n] = float(time_name_match.group(1)[2:])
        else:
            times.reverse()
            try:
                time_dict = {times[i]: float(times[i+1]) for i in range(0, len(times), 2)}
                time_dict['total'] = sum(time_dict.values())
            except Exception as e:
                print("Exception: {}".format(str(e)))
                print("Invalid times line: \"{}\"".format(lines[0]))
                time_dict = {s: 0 for s in time_keys}
        # print('time_dict: {}'.format(time_dict))
        return time_dict
    # def times_from_lines(self, lines: List[str]) -> Dict[str,float]:
    #     times = lines[0].split()
    #     times.reverse()
    #     try:
    #         time_dict = {times[i]: float(times[i+1]) for i in range(0, len(times), 2)}
    #         time_dict['total'] = sum(time_dict.values())
    #     except Exception as e:
    #         print("Exception: {}".format(str(e)))
    #         print("Invalid times line: \"{}\"".format(lines[0]))
    #         time_dict = {s: 0 for s in time_keys}
    #     return time_dict

    def compare_outputs(self, s: Scenario, sn: int, xsearch_output) -> bool:
        nonmatching = nonmatching_outputs(xsearch_output)
        if nonmatching:
            xs = []
            if len(nonmatching) == 2:
                xs = sorted(nonmatching.keys())
            elif len(nonmatching) > 2:
                xs = sorted([x for x in nonmatching.keys() if len(nonmatching[x]) > 1])
            print()
            for x in xs:
                for y in sorted(nonmatching[x]):
                    print('\n{} output != {} output for args: {}'.format(x, y, ' '.join(s.args)))
                    print('{} output:\n"{}"'.format(x, xsearch_output[x]))
                    print('{} output:\n"{}"'.format(y, xsearch_output[y]))
                    self.diff_outputs.append((sn, x, y))
            return False
        else:
            print('\nOutputs of all versions match')
            return True

    def compare_output_lens(self, sn: int, xfind_output) -> bool:
        nonmatching = nonmatching_lens(xfind_output)
        if nonmatching:
            xs = []
            if len(nonmatching) == 2:
                xs = sorted(nonmatching.keys())
            elif len(nonmatching) > 2:
                xs = sorted([x for x in nonmatching.keys() if len(nonmatching[x]) > 1])
            print()
            for x in xs:
                for y in sorted(nonmatching[x]):
                    print('{} output != {} output'.format(x, y))
                    print('{} output:\n"{}"'.format(x, xfind_output[x]))
                    print('{} output:\n"{}"'.format(y, xfind_output[y]))
                    self.diff_outputs.append((sn, x, y))
            for x in xs:
                if nonmatching[x]:
                    print('\n{} output differs with output of {} other language versions: {}'.format(x, len(nonmatching[x]), str(nonmatching[x])))
            return False
        else:
            print('\nOutputs of all versions match')
            return True

    def do_run(self, s: Scenario, sn: int, rn: int) -> RunResult:
        # return self.do_run_seq(s, sn, rn)
        return self.do_run_concurrent(s, sn, rn)

    def do_run_concurrent(self, s: Scenario, sn: int, rn: int) -> RunResult:
        """This run version starts procs for all language versions before going back and 
           capturing their outputs
        """
        xsearch_procs = {}
        xsearch_output = {}
        xsearch_times = {}
        lang_results = []
        for x in self.xsearch_names:
            fullargs = ['time', x] + s.args
            print(' '.join(fullargs[1:]))
            xsearch_procs[x] = subprocess.Popen(fullargs, bufsize=-1, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE)
            # print('process opened for {}'.format(x))

        for x in self.xsearch_names:
            p = xsearch_procs[x]
            output_lines = []
            time_lines = []
            while True:
                output_line = p.stdout.readline()
                time_line = p.stderr.readline()
                if not output_line and not time_line:
                    break
                if output_line:
                    output_lines.append(output_line.decode().strip())
                if time_line:
                    time_line = time_line.decode().strip()
                    if time_line == 'Command exited with non-zero status 1':
                        continue
                    # print('time_line: "{}"'.format(time_line))
                    time_lines.append(time_line)
            p.terminate()
            # output = '\n'.join(output_lines)
            # Temporary: sort output lines to reduce mismatches
            output = '\n'.join(sorted(output_lines))
            if s.replace_xsearch_name:
                output = xsearch_name_regex.sub('xsearch', output)
            xsearch_output[x] = output
            if self.debug:
                print('{} output:\n"{}"'.format(x, output))
            xsearch_times[x] = self.times_from_lines(time_lines)
            time_dict = xsearch_times[x]
            treal = time_dict['real'] if 'real' in time_dict else time_dict['elapsed']
            tsys = time_dict['sys'] if 'sys' in time_dict else time_dict['system']
            lang_results.append(LangResult(x, real=treal, sys=tsys, user=time_dict['user']))
        if not self.compare_outputs(s, sn, xsearch_output) and self.exit_on_diff:
            if not self.compare_output_lens(sn, xsearch_output) and self.exit_on_sort_diff:
                raise KeyboardInterrupt
        return RunResult(scenario=s, run=rn, lang_results=lang_results)

    def do_run_seq(self, s: Scenario, sn: int, rn: int) -> RunResult:
        """This run version runs each language version subprocess sequentially
        """
        xsearch_output = {}
        xsearch_times = {}
        lang_results = []
        for x in self.xsearch_names:
            fullargs = ['time', x] + s.args
            print(' '.join(fullargs[1:]))
            p = subprocess.Popen(fullargs, bufsize=-1, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE)
            # print('process opened')
            output_lines = []
            time_lines = []
            while True:
                output_line = p.stdout.readline()
                time_line = p.stderr.readline()
                if output_line == '' and time_line == '':
                    break
                if output_line != '':
                    output_lines.append(output_line.decode())
                if time_line != '':
                    time_lines.append(time_line.strip().decode())
            output = ''.join(output_lines)
            if s.replace_xsearch_name:
                output = xsearch_name_regex.sub('xsearch', output)
            xsearch_output[x] = output
            if self.debug:
                print('output:\n"{}"'.format(output))
            xsearch_times[x] = self.times_from_lines(time_lines)
            time_dict = xsearch_times[x]
            lang_results.append(LangResult(x, real=time_dict['real'], sys=time_dict['sys'], user=time_dict['user']))
        if not self.compare_outputs(s, sn, xsearch_output) and self.exit_on_diff:
            if not self.compare_output_lens(sn, xsearch_output) and self.exit_on_sort_diff:
                raise KeyboardInterrupt
        return RunResult(scenario=s, run=rn, lang_results=lang_results)

    def activate_pyvenv(self):
        if 'VIRTUAL_ENV' not in os.environ:
            print('ERROR: venv must be activated to run pysearch')
            print('Run "source ./pyvenv_setup.sh" before this script')
            sys.exit(1)

    def run(self):
        if 'pysearch' in self.xsearch_names:
            self.activate_pyvenv()
        scenario_results = ScenarioResults()
        runs = 0
        try:
            for i,s in enumerate(self.scenarios):
                sn = i + 1
                s_results = []
                for r in range(self.runs):
                    rn = r+1
                    print('\nscenario {} ("{}") run {}\n'.format(sn, s.name, rn))
                    result = self.do_run(s, sn, rn)
                    runs += 1
                    s_results.append(result)
                    self.print_run_result(sn, result)
                scenario_result = ScenarioResult(scenario=s, index=sn, run_results=s_results)
                scenario_results.append(scenario_result)
                self.print_scenario_result(scenario_result)
        except KeyboardInterrupt:
            print('\n')

        if self.diff_outputs:
            print('\nThere were output differences in some scenarios')
        else:
            print('\nOutputs of all versions in all scenarios match')

        self.print_scenario_results(scenario_results)
        # self.print_scenario_summary(scenario_results)


########################################
# Main functions
########################################
def get_args(args):
    xsearch_names = all_xsearch_names
    runs = default_runs
    debug = False
    exit_on_diff = False
    while args:
        arg = args.pop(0)
        if arg.startswith('-'):
            if arg == '-l': # xsearch_names
                xsearch_names = []
                if args:
                    langs = sorted(args.pop(0).split(','))
                    for lang in langs:
                        if lang in xsearch_dict:
                            xsearch_names.append(xsearch_dict[lang])
                        else:
                            print('Skipping unknown language: {}'.format(lang))
                else:
                    print('ERROR: missing language names for -l arg')
                    sys.exit(1)
            elif arg == '-r': # runs
                if args:
                    runs = int(args.pop(0))
                else:
                    print('ERROR: missing runs value for -r arg')
                    sys.exit(1)
            elif arg == '-b':
                exit_on_diff = True
            elif arg == '--debug':
                debug = True
            else:
                print('ERROR: unknown arg: {}'.format(arg))
                sys.exit(1)
        else:
            print('ERROR: unknown arg: {}'.format(arg))
            sys.exit(1)
    return xsearch_names, runs, exit_on_diff, debug


def main():
    xsearch_names, runs, exit_on_diff, debug = get_args(sys.argv[1:])
    print('xsearch_names: {}'.format(str(xsearch_names)))
    print('runs: {}'.format(runs))
    print('exit_on_diff: {}'.format(exit_on_diff))
    print('debug: {}'.format(debug))
    benchmarker = Benchmarker(xsearch_names=xsearch_names, runs=runs,
        scenarios=scenarios, exit_on_diff=exit_on_diff, debug=debug)
    benchmarker.run()


if __name__ == '__main__':
    main()
