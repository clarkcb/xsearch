#!/usr/bin/env python
# -*- coding: utf-8 -*-
################################################################################
#
# benchmark.py
#
# A simple benchmarking tool for the various xsearch language versions
#
################################################################################
from pprint import pprint
import subprocess
import sys
import time

########################################
# Configuration
########################################
xsearch_names = '''cljsearch cssearch gosearch hssearch javasearch
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
def print_totals(totals_dict, s, r):
    #print 'totals_dict:'
    #pprint(totals_dict)
    print "\nTotal results for %d scenarios with %d runs (%d total runs)" % \
        (s, r, s * r)
    longest = max([len(x) for x in xsearch_names])
    hdr = ['real', 'r.avg', 'sys', 's.avg', 'user', 'u.avg', 'total', 't.avg', 'rank']
    hdr_places = ' %6s' * len(hdr)
    hdr_format = ' %%-%ds %s' % (longest, hdr_places)
    hdr_line = hdr_format % tuple(['xsearch'] + hdr)
    sep_line = '-' * len(hdr_line)
    print
    print hdr_line
    print sep_line
    totals = [v['total'] for v in totals_dict.values()]
    totals.sort()
    for x in xsearch_names:
        val_places = ' %6.2f' * (len(hdr) - 1)
        line_format = ' %%-%ds %s %%6d' % (longest, val_places)
        xr = totals_dict[x]['real']
        xra = xr / (s * r)
        xs = totals_dict[x]['sys']
        xsa = xs / (s * r)
        xu = totals_dict[x]['user']
        xua = xu / (s * r)
        xt = totals_dict[x]['total']
        xta = xt / (s * r)
        vals = [x, xr, xra, xs, xsa, xu, xua, xt, xta, totals.index(xt) + 1]
        line = line_format % tuple(vals)
        print line
    print

def print_run_results(results, args, s, r):
    print "\nResults for scenario %d run %d" % (s, r)
    longest = max([len(x) for x in xsearch_names])
    hdr = ['real', 'sys', 'user', 'total']
    hdr_format = ' %%-%ds  %%6s %%6s %%6s %%6s %%6s' % longest
    hdr_line = hdr_format % tuple(['xsearch'] + hdr + ['rank'])
    sep_line = '-' * len(hdr_line)
    print
    print hdr_line
    print sep_line
    totals_dict = {}
    for x in xsearch_names:
        totals_dict[x] = results[x]['total']
    totals = totals_dict.values()
    totals.sort()
    for x in xsearch_names:
        line_format = ' %%-%ds  %%6.2f %%6.2f %%6.2f %%6.2f %%6d' % longest
        nums = []
        for h in hdr:
            nums.append(results[x][h])
        line = line_format % tuple([x] + nums + [totals.index(totals_dict[x]) + 1])
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

def run(args):
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
        xsearch_times[x] = times_from_lines(time_lines)
    verify_outputs(xsearch_output)
    return (xsearch_times, args)


########################################
# Main
########################################
def main():
    keys = ['real', 'sys', 'user', 'total']
    totals_dict = { x: {s: 0 for s in keys} for x in xsearch_names }
    for i,s in enumerate(scenarios):
        for r in range(runs):
            print '\nscenario %d run %d\n' % (i+1, r+1)
            (xsearch_times, args) = run(s)
            for x in xsearch_names:
                for k in keys:
                    totals_dict[x][k] += xsearch_times[x][k]
            print_run_results(xsearch_times, args, i+1, r+1)
    print_totals(totals_dict, len(scenarios), runs)


if __name__ == '__main__':
    main()
