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

#exts = '''clj cs go hs java js pl php py rb scala'''.split()
exts = '''clj js py rb scala'''.split()

xsearch_names = '''cssearch gosearch hssearch javasearch
                   nodesearch plsearch.pl phpsearch.php pysearch.py
                   rbsearch.rb scalasearch'''.split()

def print_totals(totals_dict, args, runs):
    print "Total results for xsearch with args %s" % str(args)
    longest = max([len(x) for x in xsearch_names])
    hdr = ['total', 'avg']
    hdr_format = ' %%-%ds  %%6s %%6s %%6s' % longest
    hdr_line = hdr_format % tuple(['xsearch'] + hdr + ['rank'])
    sep_line = '-' * len(hdr_line)
    print
    print hdr_line
    print sep_line
    totals = totals_dict.values()
    totals.sort()
    for x in xsearch_names:
        line_format = ' %%-%ds  %%6.2f %%6.2f %%6d' % longest
        vals = [x, totals_dict[x], totals_dict[x] / runs, totals.index(totals_dict[x]) + 1]
        line = line_format % tuple(vals)
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

def verify_outputs(xsearch_output):
    #print 'xsearch_output:'
    #pprint(xsearch_output)
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
        for (x,y) in nonmatching:
            print '%s output != %s output' % (x, y)
    else:
        print 'Output of all versions matches'

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
                #print output_line
                output_lines.append(output_line)
            if time_line != '':
                #print time_line
                time_lines.append(time_line.strip())
        cmd = ' '.join(fullargs)
        xsearch_output[x] = output_lines
        xsearch_times[x] = times_from_lines(time_lines)
        #time.sleep(1)
    verify_outputs(xsearch_output)
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
    print_totals(totals_dict, args, runs)


if __name__ == '__main__':
    main()
