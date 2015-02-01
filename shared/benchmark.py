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

def print_run_results(results, args):
    print "\nResults for xsearch with args %s" % str(args)
    longest = max([len(x) for x in xsearch_names])
    hdr = ['real', 'sys', 'user', 'total']
    hdr_format = ' %%-%ds  %%6s %%6s %%6s %%6s %%6s' % longest
    #print "hdr_format: %s" % hdr_format
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
            #print line
            if line != '':
                output_lines.append(line)
            if time != '':
                time_lines.append(time.strip())
        cmd = ' '.join(fullargs)
        #print cmd
        xsearch_output[x] = output_lines
        xsearch_times[x] = times_from_lines(time_lines)
    print
    #pprint(xsearch_output)
    #pprint(xsearch_times)
    print_run_results(xsearch_times, args)

def run1():
    searchpattern='Searcher'
    startpath='/Users/cary/src/git/xsearch/'    
    args = ['-x', ','.join(exts), '-s', searchpattern, startpath]
    run(args)

def main():
    runs = 10
    for r in range(runs):
        print 'run %d' % (r+1)
        run1()

if __name__ == '__main__':
    main()
