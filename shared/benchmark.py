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

#exts = '''clj cs go hs java js pl php py rb scala'''.split()
exts = '''clj hs js py rb scala'''.split()

xsearch_names = '''cljsearch cssearch gosearch hssearch javasearch
                   nodesearch plsearch.pl phpsearch.php pysearch.py
                   rbsearch.rb scalasearch'''.split()

def print_totals(totals_dict, args):
    print "Total results for xsearch with args %s" % str(args)
    longest = max([len(x) for x in xsearch_names])
    hdr = ['total']
    hdr_format = ' %%-%ds  %%6s %%6s' % longest
    hdr_line = hdr_format % tuple(['xsearch'] + hdr + ['rank'])
    sep_line = '-' * len(hdr_line)
    print
    print hdr_line
    print sep_line
    totals = totals_dict.values()
    totals.sort()
    for x in xsearch_names:
        line_format = ' %%-%ds  %%6.2f %%6d' % longest
        nums = []
        for h in hdr:
            nums.append(totals_dict[x])
        line = line_format % tuple([x] + nums + [totals.index(totals_dict[x]) + 1])
        print line
    print

def print_run_results(results, args):
    print "Results for xsearch with args %s" % str(args)
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
    time_dict = {times[i]: float(times[i+1]) for i in range(0, len(times), 2)}
    time_dict['total'] = sum(time_dict.values())
    return time_dict

def run(args):
    xsearch_output = {}
    xsearch_times = {}
    for x in xsearch_names:
        fullargs = ['time', x] + args
        p = subprocess.Popen(fullargs, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output_lines = []
        time_lines = []
        times = {}
        while True:
            line = p.stdout.readline()
            time = p.stderr.readline()
            if line == '' and time == '':
                break
            if line != '':
                output_lines.append(line)
            if time != '':
                time_lines.append(time.strip())
        cmd = ' '.join(fullargs)
        xsearch_output[x] = output_lines
        xsearch_times[x] = times_from_lines(time_lines)
    print
    return (xsearch_times, args)

def scenario1():
    searchpattern='Searcher'
    startpath='/Users/cary/src/git/xsearch/'    
    args = ['-x', ','.join(exts), '-s', searchpattern, startpath]
    return run(args)

def main():
    runs = 10
    totals_dict = {x: 0 for x in xsearch_names}
    for r in range(runs):
        print 'scenario1 run %d' % (r+1)
        (xsearch_times, args) = scenario1()
        for x in xsearch_names:
            totals_dict[x] += xsearch_times[x]['total']
        print_run_results(xsearch_times, args)
    print_totals(totals_dict, args)


if __name__ == '__main__':
    main()
