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
from pprint import pprint
import subprocess
import sys
import time

########################################
# Configuration
########################################
xsearch_names = '''cljsearch cssearch fssearch gosearch hssearch javasearch
                   nodesearch plsearch.pl phpsearch.php pysearch.py
                   rbsearch.rb scalasearch'''.split()

#exts = ','.join('clj cs go hs java js pl php py rb scala'.split())
exts = ','.join('py rb'.split())

runs = 10

scenarios = [
    ['-x', exts, '-s', 'Searcher', '/Users/cary/src/git/xsearch/'],
    ['-x', exts, '-s', 'Searcher', '/Users/cary/src/git/xsearch/', '-m']
]


########################################
# Functions
########################################
RunResult = namedtuple('RunResult', ['s', 'r', 'time_dict'], verbose=False)

def print_totals(totals_dict, s, r):
    #print 'totals_dict:'
    #pprint(totals_dict)
    print "\nTotal results for %d scenarios with %d runs (%d total runs)" % \
        (s, r, s * r)
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
        xra = xr / (s * r)
        xrr = reals.index(xr) + 1
        xs = totals_dict[x]['sys']
        xsa = xs / (s * r)
        xsr = syss.index(xs) + 1
        xu = totals_dict[x]['user']
        xua = xu / (s * r)
        xur = users.index(xu) + 1
        xt = totals_dict[x]['total']
        xta = xt / (s * r)
        xtr = totals.index(xt) + 1
        vals = [x, xr, xra, xrr, xs, xsa, xsr, xu, xua, xur, xt, xta, xtr]
        line = line_format % tuple(vals)
        print line
    print

def print_run_results(result):
    print "\nResults for scenario %d run %d" % (result.s, result.r)
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

def verify_outputs(xsearch_output):
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

def run(sn, rn, args):
    xsearch_output = {}
    xsearch_times = {}
    for x in xsearch_names:
        fullargs = ['time', x] + args
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
    verify_outputs(xsearch_output)
    result = RunResult(s=sn, r=rn, time_dict=xsearch_times)
    return result


########################################
# Main
########################################
def main():
    keys = ['real', 'sys', 'user', 'total']
    totals_dict = { x: {s: 0 for s in keys} for x in xsearch_names }
    results = []
    for i,s in enumerate(scenarios):
        for r in range(runs):
            sn = i+1
            rn = r+1
            print '\nscenario %d run %d\n' % (sn, rn)
            result = run(sn, rn, s)
            results.append(result)
            for x in xsearch_names:
                for k in keys:
                    totals_dict[x][k] += result.time_dict[x][k]
            print_run_results(result)
    print_totals(totals_dict, len(scenarios), runs)


if __name__ == '__main__':
    main()
