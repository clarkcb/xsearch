#!/usr/bin/env python
# -*- coding: utf-8 -*-
################################################################################
#
# benchmark.py
#
# A simple benchmarking tool for the various xsearch language versions
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
xsearch_names = '''cljsearch cssearch fssearch gosearch hssearch javasearch
                   nodesearch plsearch.pl phpsearch.php pysearch.py
                   rbsearch.rb scalasearch'''.split()

#exts = ','.join('clj cs go hs java js pl php py rb scala'.split())
exts = ','.join('py rb'.split())

runs = 10

startpath = os.path.expanduser('~/src/git/xsearch/')

scenarios = [
    Scenario('help', ['-h'], False), # display help/usage, don't compare output
    Scenario('search lines #1', ['-x', exts, '-s', 'Searcher', startpath], True),
    Scenario('search contents #1', ['-x', exts, '-s', 'Searcher', startpath, '-m'], True)
]

time_keys = ['real', 'sys', 'user', 'total']


########################################
# Functions
########################################
def print_totals_dict(totals_dict):
    #print 'totals_dict:'
    #pprint(totals_dict)
    s = len(scenarios)
    total_runs = s * runs
    longest = max([len(x) for x in xsearch_names])
    hdr = ['real', 'r.avg', 'r.rank', 'sys', 's.avg', 's.rank', 'user',
           'u.avg','u.rank',  'total', 't.avg', 't.rank']
    col_width = max([len(h) for h in hdr]) + 1
    hdr_place = ' %' + str(col_width) + 's'
    hdr_places = hdr_place * len(hdr)
    hdr_format = ' %%-%ds %s' % (longest, hdr_places)
    hdr_line = hdr_format % tuple(['xsearch'] + hdr)
    sep_line = '-' * len(hdr_line)
    print
    print hdr_line
    print sep_line
    reals = [v['real'] for v in totals_dict.values()]
    syss = [v['sys'] for v in totals_dict.values()]
    users = [v['user'] for v in totals_dict.values()]
    totals = [v['total'] for v in totals_dict.values()]
    reals.sort()
    syss.sort()
    users.sort()
    totals.sort()
    for x in xsearch_names:
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
        print line
    print

def print_totals(results):
    s = len(scenarios)
    total_runs = s * runs
    print "\nTotal results for %d scenarios with %d runs (%d total runs)" % \
        (s, runs, total_runs)
    totals_dict = { x: {s: 0 for s in time_keys} for x in xsearch_names }
    for r in results:
        for x in xsearch_names:
            for k in time_keys:
                totals_dict[x][k] += r.time_dict[x][k]
    print_totals_dict(totals_dict)

def print_scenario_totals(s, sn, results):
    print '\nTotal results for scenario %d ("%s") with %d runs' % \
        (sn, s.name, runs)
    totals_dict = { x: {s: 0 for s in time_keys} for x in xsearch_names }
    for r in results:
        for x in xsearch_names:
            for k in time_keys:
                totals_dict[x][k] += r.time_dict[x][k]
    print_totals_dict(totals_dict)

def print_run_results(sn, result):
    print '\nResults for scenario %d ("%s") run %d' % (sn, result.scenario.name, result.run)
    longest = max([len(x) for x in xsearch_names])
    hdr = ['real', 'r.rank', 'sys', 's.rank', 'user', 'u.rank', 'total', 't.rank']
    col_width = max([len(h) for h in hdr]) + 1
    hdr_place = ' %' + str(col_width) + 's'
    hdr_places = hdr_place * len(hdr)
    hdr_format = ' %%-%ds %s' % (longest, hdr_places)
    hdr_line = hdr_format % tuple(['xsearch'] + hdr)
    sep_line = '-' * len(hdr_line)
    print
    print hdr_line
    print sep_line
    reals = [v['real'] for v in result.time_dict.values()]
    syss = [v['sys'] for v in result.time_dict.values()]
    users = [v['user'] for v in result.time_dict.values()]
    totals = [v['total'] for v in result.time_dict.values()]
    reals.sort()
    syss.sort()
    users.sort()
    totals.sort()
    for x in xsearch_names:
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
        print line
    print

def times_from_lines(lines):
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

def compare_outputs(xsearch_output):
    nonmatching = []
    for x in xsearch_names:
        for y in [y for y in xsearch_output.keys() if y != x]:
            x_output = xsearch_output[x]
            y_output = xsearch_output[y]
            x_output.sort()
            y_output.sort()
            if x_output != y_output:
                nonmatching.append((x, y))
        del(xsearch_output[x])
    if nonmatching:
        print
        for (x,y) in nonmatching:
            print '%s output != %s output' % (x, y)
    else:
        print '\nOutput of all versions matches'

def run(s, sn, rn):
    xsearch_output = {}
    xsearch_times = {}
    for x in xsearch_names:
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
        cmd = ' '.join(fullargs)
        xsearch_output[x] = output_lines
        #print 'output_lines (%d):' % len(output_lines)
        #pprint(output_lines)
        xsearch_times[x] = times_from_lines(time_lines)
    if s.compare_output:
        compare_outputs(xsearch_output)
    return RunResult(scenario=s, run=rn, time_dict=xsearch_times)


########################################
# Main
########################################
def main():
    results = []
    for i,s in enumerate(scenarios):
        sn = i+1
        s_results = []
        for r in range(runs):
            rn = r+1
            print '\nscenario %d ("%s") run %d\n' % (sn, s.name, rn)
            result = run(s, sn, rn)
            s_results.append(result)
            print_run_results(sn, result)
        print_scenario_totals(s, sn, s_results)
        results.extend(s_results)
    print_totals(results)


if __name__ == '__main__':
    main()
