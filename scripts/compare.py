#!/usr/bin/env python
# -*- coding: utf-8 -*-
################################################################################
#
# compare.py
#
# A simple tool to compare the output of various search scenarios across
# language versions
#
################################################################################
from cStringIO import StringIO
import subprocess
import sys

from xsearch import *


########################################
# Configuration
########################################
#exts = ','.join('clj cs go hs java js pl php py rb scala'.split())
exts = 'cs,swift'
startpath = default_startpath
invalid_searchpattern = '"ZZYZZYZZY"'
valid_searchpattern = '"Searcher"'
utf8bom_searchpattern = r'"\xef\xbb\xbf"'

scenarios = [
    # Invalid / help scenarios
    Scenario('no args', [], True),
    # NON-MATCHING Scenario('debug only', ['--debug'], True),
    Scenario('no startpath', ['-x', exts, '-s', valid_searchpattern], True),
    Scenario('invalid startpath', ['-x', exts, '-s', valid_searchpattern, '/invalid/startpath'], True),
    Scenario('no searchpatterns', ['-x', exts, startpath], True),
    Scenario('invalid argument', ['-x', exts, startpath, '-Q'], True),
    Scenario('help', ['-h'], True),
    # NON-MATCHING Scenario('help with debug', ['-h', '--debug'], True),
    Scenario('search lines, invalid search pattern', ['-x', exts, '-s', invalid_searchpattern, '-F', 'compare', startpath], False),
    Scenario('search contents, invalid search pattern', ['-x', exts, '-s', invalid_searchpattern, '-F', 'compare', startpath, '-m'], False),

    # Valid search patterns
    Scenario('search lines, valid search pattern', ['-x', exts, '-s', valid_searchpattern, startpath], False),
    Scenario('search contents, valid search pattern', ['-x', exts, '-s', valid_searchpattern, startpath, '-m'], False),
    Scenario('search lines, valid search pattern, first match', ['-x', exts, '-s', valid_searchpattern, startpath, '-1'], False),
    Scenario('search contents, valid search pattern, first match', ['-x', exts, '-s', valid_searchpattern, startpath, '-m', '-1'], False),
    # NON-MATCHING Scenario('search lines, valid search pattern, 2 lines before', ['-x', exts, '-s', valid_searchpattern, startpath, '-l', '2'], False),
    # NON-MATCHING Scenario('search contents, valid search pattern, 2 lines before', ['-x', exts, '-s', valid_searchpattern, startpath, '-m',  '-l', '2'], False),

    # List dirs, files, lines
    Scenario('listdirs', ['-x', exts, '-s', valid_searchpattern, startpath, '-P', '--listdirs'], False),
    Scenario('listfiles', ['-x', exts, '-s', valid_searchpattern, startpath, '-P', '--listfiles'], False),
    Scenario('listlines', ['-x', exts, '-s', valid_searchpattern, startpath, '-P', '--listlines'], False),
    Scenario('listlines + unique', ['-x', exts, '-s', valid_searchpattern, startpath, '-P', '--listlines', '-u'], False),
    Scenario('list all', ['-x', exts, '-s', valid_searchpattern, startpath, '-P', '--listdirs', '--listfiles', '--listlines'], False),
    Scenario('list all + unique', ['-x', exts, '-s', valid_searchpattern, startpath, '-P', '--listdirs', '--listfiles', '--listlines', '-u'], False),

    # Some special cases
    Scenario('search contents, UTF-8 BOM search pattern', ['-x', 'cs,fs', '-s', utf8bom_searchpattern, startpath], False),
]


########################################
# Comparator class
########################################
class Comparator(object):
    def __init__(self, **kargs):
        self.xsearch_names = all_xsearch_names
        self.scenarios = []
        self.debug = False
        self.__dict__.update(kargs)
        self.results = [] # a list of tuples of (scenario, {nonmatching})

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
        return nonmatching

    def run_scenario(self, scenario, sn):
        xsearch_output = {}
        for x in self.xsearch_names:
            fullargs = [x] + scenario.args
            print ' '.join(fullargs)
            p = subprocess.Popen(fullargs, bufsize=-1, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE)
            output_lines = []
            while True:
                output_line = p.stdout.readline()
                if output_line == '':
                    break
                output_lines.append(output_line)
            output = ''.join(output_lines)
            if scenario.replace_xsearch_name:
                output = xsearch_name_regex.sub('xsearch', output)
            xsearch_output[x] = output
            if self.debug:
                print 'output:\n"%s"' % output
        nonmatching = self.compare_outputs(xsearch_output)
        self.results.append((scenario, nonmatching))

    def run(self):
        results = []
        hdr_len = 80
        for i,s in enumerate(self.scenarios):
            sn = i+1
            print '\n\n%s' % ('=' * hdr_len)
            print 'scenario %d: %s' % (sn, s.name)
            print '%s\n' % ('-' * hdr_len)
            self.run_scenario(s, sn)
        nonmatching_results = [r for r in self.results if r[1]]
        if nonmatching_results:
            print '\nFound non-matching output in these scenarios:'
            for r in nonmatching_results:
                print ' - %s' % r[0].name
        else:
            print '\nOutputs matched for all xsearch versions in all scenarios'


########################################
# Main functions
########################################
def get_args(args):
    xsearch_names = all_xsearch_names
    debug = False
    while args:
        arg = args.pop(0)
        if arg.startswith('-'):
            if arg == '-l': # add xsearch_names
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
            elif arg == '-L': # remove xsearch_names
                if args:
                    langs = sorted(args.pop(0).split(','))
                    for lang in langs:
                        if lang in xsearch_dict and xsearch_dict[lang] in xsearch_names:
                            xsearch_names.remove(xsearch_dict[lang])
                else:
                    print 'ERROR: missing language names for -L arg'
                    sys.exit(1)
            elif arg == '--debug':
                debug = True
            else:
                print 'ERROR: unknown arg: %s' % arg
                sys.exit(1)

        else:
            print 'ERROR: unknown arg: %s' % arg
            sys.exit(1)
    return xsearch_names, debug

def main():
    xsearch_names, debug = get_args(sys.argv[1:])
    print 'xsearch_names: %s' % str(xsearch_names)
    print 'scenarios: %d' % len(scenarios)
    comparator = Comparator(xsearch_names=xsearch_names,
        scenarios=scenarios, debug=debug)
    comparator.run()


if __name__ == '__main__':
    main()
