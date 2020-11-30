#!/usr/bin/env python3
# -*- coding: utf-8 -*-
################################################################################
#
# benchmark.py
#
# A simple benchmarking tool for the various xsearch language versions
#
################################################################################
from io import StringIO
import os
import subprocess
import sys
import time

from xsearch import *


########################################
# Configuration
########################################
#exts = ','.join('clj cs go hs java js pl php py rb scala'.split())
exts = ','.join('py rb'.split())

startpath = os.path.join(XSEARCHPATH, 'python')

default_runs = 10

scenarios = [
    Scenario('no args', [], True),
    Scenario('help', ['-h'], True),
    Scenario('search lines #1', ['-x', exts, '-s', 'Searcher', startpath], False),
    Scenario('search contents #1', ['-x', exts, '-s', 'Searcher', startpath, '-m'], False),
    Scenario('search lines #2 - first match', ['-x', exts, '-s', 'Searcher', startpath, '-1'], False),
    Scenario('search contents #2 - first match', ['-x', exts, '-s', 'Searcher', startpath, '-m', '-1'], False)
]

time_keys = ['real', 'sys', 'user', 'total']


########################################
# Benchmarker class
########################################
class Benchmarker(object):
    def __init__(self, **kwargs):
        self.xsearch_names = all_xsearch_names
        self.scenarios = []
        self.runs = default_runs
        self.debug = True
        self.diff_outputs = []
        self.__dict__.update(kwargs)

    def print_totals_dict(self, totals_dict, total_runs=default_runs):
        sio = StringIO()
        longest = max([len(x) for x in self.xsearch_names])
        hdr = ['real', 'avg', 'rank', 'sys', 'avg', 'rank', 'user',
               'avg', 'rank', 'total', 'avg', 'rank']
        col_width = max([len(h) for h in hdr]) + 1
        hdr_place = ' %' + str(col_width) + 's'
        hdr_places = hdr_place * len(hdr)
        hdr_format = ' %%-%ds %s' % (longest, hdr_places)
        hdr_line = hdr_format % tuple(['xsearch'] + hdr)
        sep_line = '-' * len(hdr_line)
        sio.write("\n")
        sio.write("{}\n".format(hdr_line))
        sio.write("{}\n".format(sep_line))
        reals = sorted([v['real'] for v in totals_dict.values()])
        syss = sorted([v['sys'] for v in totals_dict.values()])
        users = sorted([v['user'] for v in totals_dict.values()])
        totals = sorted([v['total'] for v in totals_dict.values()])
        time_place = ' %' + str(col_width) + '.2f'
        rank_place = ' %' + str(col_width) + 'd'
        val_places = (time_place + time_place + rank_place) * 4
        line_format = ' %%-%ds %s' % (longest, val_places)
        for x in self.xsearch_names:
            xr = totals_dict[x]['real']
            xra = xr / total_runs
            xrr = reals.index(xr) + 1
            xs = totals_dict[x]['sys']
            xsa = xs / total_runs
            xsr = syss.index(xs) + 1
            xu = totals_dict[x]['user']
            xua = xu / total_runs
            xur = users.index(xu) + 1
            xt = totals_dict[x]['total']
            xta = xt / total_runs
            xtr = totals.index(xt) + 1
            vals = [x, xr, xra, xrr, xs, xsa, xsr, xu, xua, xur, xt, xta, xtr]
            line = line_format % tuple(vals)
            sio.write("{}\n".format(line))
        print(sio.getvalue())

    def print_totals(self, results, last_scenario, total_runs):
        if self.diff_outputs:
            print("\nOutput differences:")
            for d in self.diff_outputs:
                print("scenario {}: {} differs from {}".format(d[0], d[1], d[2]))
        else:
            print("\nAll outputs of all scenarios match")
        total_scenarios = len(self.scenarios)
        max_runs = total_scenarios * self.runs
        print("\nTotal results for {} out of {} scenarios with {} out of {} total runs".
            format(last_scenario, total_scenarios, total_runs, max_runs))
        totals_dict = { x: {s: 0 for s in time_keys} for x in self.xsearch_names }
        for r in results:
            for x in self.xsearch_names:
                for k in time_keys:
                    totals_dict[x][k] += r.time_dict[x][k]
        self.print_totals_dict(totals_dict, total_runs)

    def print_scenario_totals(self, s, sn, results):
        print('\nTotal results for scenario {} ("{}") with {} runs'.
            format(sn, s.name, self.runs))
        totals_dict = { x: {s: 0 for s in time_keys} for x in self.xsearch_names }
        for r in results:
            for x in self.xsearch_names:
                for k in time_keys:
                    totals_dict[x][k] += r.time_dict[x][k]
        self.print_totals_dict(totals_dict)

    def print_run_results(self, sn, result):
        sio = StringIO()
        sio.write('\nResults for scenario {} ("{}") run {}'.format(sn, result.scenario.name, result.run))
        longest = max([len(x) for x in self.xsearch_names])
        hdr = ['real', 'rank', 'sys', 'rank', 'user', 'rank', 'total', 'rank']
        col_width = max([len(h) for h in hdr]) + 1
        hdr_place = ' %' + str(col_width) + 's'
        hdr_places = hdr_place * len(hdr)
        hdr_format = ' %%-%ds %s' % (longest, hdr_places)
        hdr_line = hdr_format % tuple(['xsearch'] + hdr)
        sep_line = '-' * len(hdr_line)
        sio.write("\n")
        sio.write("{}\n".format(hdr_line))
        sio.write("{}\n".format(sep_line))
        reals = sorted([v['real'] for v in result.time_dict.values()])
        syss = sorted([v['sys'] for v in result.time_dict.values()])
        users = sorted([v['user'] for v in result.time_dict.values()])
        totals = sorted([v['total'] for v in result.time_dict.values()])
        time_place = ' %' + str(col_width) + '.2f'
        rank_place = ' %' + str(col_width) + 'd'
        val_places = (time_place + rank_place) * 4
        line_format = ' %%-%ds %s' % (longest, val_places)
        for x in self.xsearch_names:
            xr = result.time_dict[x]['real']
            xrr = reals.index(xr) + 1
            xs = result.time_dict[x]['sys']
            xsr = syss.index(xs) + 1
            xu = result.time_dict[x]['user']
            xur = users.index(xu) + 1
            xt = result.time_dict[x]['total']
            xtr = totals.index(xt) + 1
            vals = [x, xr, xrr, xs, xsr, xu, xur, xt, xtr]
            line = line_format % tuple(vals)
            sio.write("{}\n".format(line))
        print(sio.getvalue())

    def times_from_lines(self, lines):
        times = lines[0].split()
        times.reverse()
        # print(times)
        time_dict = {}
        try:
            time_dict = {times[i]: float(times[i+1]) for i in range(0, len(times), 2)}
            time_dict['total'] = sum(time_dict.values())
        except Exception as e:
            print("Exception: {}".format(str(e)))
            print("Invalid times line: \"{}\"".format(lines[0]))
            time_dict = {s: 0 for s in ['real', 'sys', 'user', 'total']}
        return time_dict

    def compare_lens(self, xsearch_output):
        nonmatching = nonmatching_lens(xsearch_output)
        if nonmatching:
            xs = []
            if len(nonmatching) == 2:
                xs = [sorted(nonmatching.keys())[0]]
            elif len(nonmatching) > 2:
                xs = sorted([x for x in nonmatching.keys() if len(nonmatching[x]) > 1])
            print()
            for x in xs:
                for y in sorted(nonmatching[x]):
                    print('{} output line length ({}) != {} output line length ({})'.format(x, lens[x], y, lens[y]))
        else:
            print('\nOutput line lengths of all versions match')

    def compare_outputs(self, sn, xsearch_output):
        # print('compare_outputs')
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
                    print('{} output != {} output'.format(x, y))
                    print('{} output:\n"{}"'.format(x, xsearch_output[x]))
                    print('{} output:\n"{}"'.format(y, xsearch_output[y]))
                    self.diff_outputs.append((sn, x, y))
            return False
        else:
            print('\nOutputs of all versions match')
            return True

    def do_run(self, s, sn, rn):
        # return self.do_run_seq(s, sn, rn)
        return self.do_run_concurrent(s, sn, rn)

    def do_run_concurrent(self, s, sn, rn):
        """This run version starts procs for all language versions before going back and 
           capturing their outputs
        """
        xsearch_procs = {}
        xsearch_output = {}
        xsearch_times = {}
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
                    time_lines.append(time_line.decode().strip())
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
        self.compare_outputs(sn, xsearch_output)
        return RunResult(scenario=s, run=rn, time_dict=xsearch_times)

    def do_run_seq(self, s, sn, rn):
        """This run version runs each language version subprocess sequentially
        """
        xsearch_output = {}
        xsearch_times = {}
        for x in self.xsearch_names:
            fullargs = ['time', x] + s.args
            print(' '.join(fullargs[1:]))
            p = subprocess.Popen(fullargs, bufsize=-1, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE)
            # print('process opened')
            output_lines = []
            time_lines = []
            times = {}
            while True:
                #print("Reading stdout")
                output_line = p.stdout.readline()
                #print('output_line: "{}"'.format(output_line))
                #print("Reading stderr")
                time_line = p.stderr.readline()
                #print('time_line: "{}"'.format(time_line))
                if output_line == '' and time_line == '':
                    break
                if output_line != '':
                    output_lines.append(output_line)
                if time_line != '':
                    time_lines.append(time_line.strip())
            output = ''.join(output_lines)
            if s.replace_xsearch_name:
                output = xsearch_name_regex.sub('xsearch', output)
            xsearch_output[x] = output
            if self.debug:
                print('output:\n"{}"'.format(output))
            xsearch_times[x] = self.times_from_lines(time_lines)
        self.compare_outputs(sn, xsearch_output)
        return RunResult(scenario=s, run=rn, time_dict=xsearch_times)

    def run(self):
        results = []
        sn = 0
        runs = 0
        try:
            for i,s in enumerate(self.scenarios):
                sn = i+1
                s_results = []
                for r in range(self.runs):
                    rn = r+1
                    print('\nscenario {} ("{}") run {}\n'.format(sn, s.name, rn))
                    result = self.do_run(s, sn, rn)
                    runs += 1
                    s_results.append(result)
                    self.print_run_results(sn, result)
                self.print_scenario_totals(s, sn, s_results)
                results.extend(s_results)
        except KeyboardInterrupt:
            print('\n')
        self.print_totals(results, sn, runs)


########################################
# Main functions
########################################
def get_args(args):
    xsearch_names = all_xsearch_names
    runs = default_runs
    debug = False
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
            elif arg == '--debug':
                debug = True
            else:
                print('ERROR: unknown arg: {}'.format(arg))
                sys.exit(1)

        else:
            print('ERROR: unknown arg: {}'.format(arg))
            sys.exit(1)
    return xsearch_names, runs, debug


def main():
    xsearch_names, runs, debug = get_args(sys.argv[1:])
    print('xsearch_names: {}'.format(str(xsearch_names)))
    print('runs: {}'.format(runs))
    benchmarker = Benchmarker(xsearch_names=xsearch_names, runs=runs,
        scenarios=scenarios, debug=debug)
    benchmarker.run()


if __name__ == '__main__':
    main()
