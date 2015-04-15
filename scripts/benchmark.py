#!/usr/bin/env python
# -*- coding: utf-8 -*-
################################################################################
#
# benchmark.py
#
# A simple benchmarking tool for the various xsearch language versions
#
################################################################################
from cStringIO import StringIO
import subprocess
import sys
import time

from xsearch import *


########################################
# Configuration
########################################
#exts = ','.join('clj cs go hs java js pl php py rb scala'.split())
exts = ','.join('py rb'.split())

startpath = default_startpath

scenarios = [
    # Scenario('no args', [], True),
    # Scenario('help', ['-h'], True),
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
    def __init__(self, **kargs):
        self.xsearch_names = all_xsearch_names
        self.scenarios = []
        self.runs = 0
        self.debug = False
        self.__dict__.update(kargs)

    def print_totals_dict(self, totals_dict):
        sio = StringIO()
        s = len(self.scenarios)
        total_runs = s * self.runs
        longest = max([len(x) for x in self.xsearch_names])
        hdr = ['real', 'avg', 'rank', 'sys', 'avg', 'rank', 'user',
               'avg','rank',  'total', 'avg', 'rank']
        col_width = max([len(h) for h in hdr]) + 1
        hdr_place = ' %' + str(col_width) + 's'
        hdr_places = hdr_place * len(hdr)
        hdr_format = ' %%-%ds %s' % (longest, hdr_places)
        hdr_line = hdr_format % tuple(['xsearch'] + hdr)
        sep_line = '-' * len(hdr_line)
        sio.write("\n")
        sio.write("%s\n" % hdr_line)
        sio.write("%s\n" % sep_line)
        reals = sorted([v['real'] for v in totals_dict.values()])
        syss = sorted([v['sys'] for v in totals_dict.values()])
        users = sorted([v['user'] for v in totals_dict.values()])
        totals = sorted([v['total'] for v in totals_dict.values()])
        for x in self.xsearch_names:
            time_place = ' %' + str(col_width) + '.2f'
            rank_place = ' %' + str(col_width) + 'd'
            val_places = (time_place + time_place + rank_place) * 4
            line_format = ' %%-%ds %s' % (longest, val_places)
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
            sio.write("%s\n" % line)
        print sio.getvalue()

    def print_totals(self, results):
        s = len(self.scenarios)
        total_runs = s * self.runs
        print "\nTotal results for %d scenarios with %d runs (%d total runs)" % \
            (s, self.runs, total_runs)
        totals_dict = { x: {s: 0 for s in time_keys} for x in self.xsearch_names }
        for r in results:
            for x in self.xsearch_names:
                for k in time_keys:
                    totals_dict[x][k] += r.time_dict[x][k]
        self.print_totals_dict(totals_dict)

    def print_scenario_totals(self, s, sn, results):
        print '\nTotal results for scenario %d ("%s") with %d runs' % \
            (sn, s.name, self.runs)
        totals_dict = { x: {s: 0 for s in time_keys} for x in self.xsearch_names }
        for r in results:
            for x in self.xsearch_names:
                for k in time_keys:
                    totals_dict[x][k] += r.time_dict[x][k]
        self.print_totals_dict(totals_dict)

    def print_run_results(self, sn, result):
        sio = StringIO()
        sio.write('\nResults for scenario %d ("%s") run %d' % (sn, result.scenario.name, result.run))
        longest = max([len(x) for x in self.xsearch_names])
        hdr = ['real', 'rank', 'sys', 'rank', 'user', 'rank', 'total', 'rank']
        col_width = max([len(h) for h in hdr]) + 1
        hdr_place = ' %' + str(col_width) + 's'
        hdr_places = hdr_place * len(hdr)
        hdr_format = ' %%-%ds %s' % (longest, hdr_places)
        hdr_line = hdr_format % tuple(['xsearch'] + hdr)
        sep_line = '-' * len(hdr_line)
        sio.write("\n")
        sio.write("%s\n" % hdr_line)
        sio.write("%s\n" % sep_line)
        reals = sorted([v['real'] for v in result.time_dict.values()])
        syss = sorted([v['sys'] for v in result.time_dict.values()])
        users = sorted([v['user'] for v in result.time_dict.values()])
        totals = sorted([v['total'] for v in result.time_dict.values()])
        for x in self.xsearch_names:
            time_place = ' %' + str(col_width) + '.2f'
            rank_place = ' %' + str(col_width) + 'd'
            val_places = (time_place + rank_place) * 4
            line_format = ' %%-%ds %s' % (longest, val_places)
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
            sio.write("%s\n" % line)
        print sio.getvalue()

    def times_from_lines(self, lines):
        times = lines[0].split()
        times.reverse()
        time_dict = {}
        try:
            time_dict = {times[i]: float(times[i+1]) for i in range(0, len(times), 2)}
            time_dict['total'] = sum(time_dict.values())
        except ValueError as e:
            print "ValueError: %s" % str(e)
            print "Invalid times line: \"%s\"" % lines[0]
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
            print
            for x in xs:
                for y in sorted(nonmatching[x]):
                    print '%s output line length (%d) != %s output line length (%d)' % (x, lens[x], y, lens[y])
        else:
            print '\nOutput line lengths of all versions match'

    def compare_outputs(self, xsearch_output):
        nonmatching = nonmatching_outputs(xsearch_output)
        if nonmatching:
            xs = []
            if len(nonmatching) == 2:
                xs = sorted(nonmatching.keys())
            elif len(nonmatching) > 2:
                xs = sorted([x for x in nonmatching.keys() if len(nonmatching[x]) > 1])
            print
            for x in xs:
                for y in sorted(nonmatching[x]):
                    print '%s output != %s output' % (x, y)
                    # print '%s output:\n"%s"' % (x, xsearch_output[x])
                    # print '%s output:\n"%s"' % (y, xsearch_output[y])
        else:
            print '\nOutputs of all versions match'

    def do_run(self, s, sn, rn):
        xsearch_output = {}
        xsearch_times = {}
        for x in self.xsearch_names:
            fullargs = ['time', x] + s.args
            print ' '.join(fullargs[1:])
            p = subprocess.Popen(fullargs, bufsize=-1, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE)
            output_lines = []
            time_lines = []
            times = {}
            while True:
                output_line = p.stdout.readline()
                time_line = p.stderr.readline()
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
                print 'output:\n"%s"' % output
            xsearch_times[x] = self.times_from_lines(time_lines)
        self.compare_outputs(xsearch_output)
        return RunResult(scenario=s, run=rn, time_dict=xsearch_times)

    def run(self):
        results = []
        for i,s in enumerate(self.scenarios):
            sn = i+1
            s_results = []
            for r in range(self.runs):
                rn = r+1
                print '\nscenario %d ("%s") run %d\n' % (sn, s.name, rn)
                result = self.do_run(s, sn, rn)
                s_results.append(result)
                self.print_run_results(sn, result)
            self.print_scenario_totals(s, sn, s_results)
            results.extend(s_results)
        self.print_totals(results)

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
                            print 'Skipping unknown language: %s' % lang
                else:
                    print 'ERROR: missing language names for -l arg'
                    sys.exit(1)
            elif arg == '-r': # runs
                if args:
                    runs = int(args.pop(0))
                else:
                    print 'ERROR: missing runs value for -r arg'
                    sys.exit(1)
            elif arg == '--debug':
                debug = True
            else:
                print 'ERROR: unknown arg: %s' % arg
                sys.exit(1)

        else:
            print 'ERROR: unknown arg: %s' % arg
            sys.exit(1)
    return xsearch_names, runs, debug


def main():
    xsearch_names, runs, debug = get_args(sys.argv[1:])
    print 'xsearch_names: %s' % str(xsearch_names)
    print 'runs: %d' % runs
    benchmarker = Benchmarker(xsearch_names=xsearch_names, runs=runs,
        scenarios=scenarios, debug=debug)
    benchmarker.run()


if __name__ == '__main__':
    main()
