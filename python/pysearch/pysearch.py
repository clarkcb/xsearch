#!/usr/bin/env python
# -*- coding: utf-8 -*-
################################################################################
#
# pysearch.py
#
# A CLI file search utility implemented in python (2.x)
#
################################################################################
import sys

from searcher import Searcher
from searchoptions import SearchOptions

def log(message):
    """log a message (for now just print to stdout)"""
    print message

def main():
    searchoptions = SearchOptions()

    settings = None
    try:
        settings = searchoptions.search_settings_from_args(sys.argv[1:])
    except Exception, e:
        log('\nException: {0!s}\n'.format(e))
        searchoptions.usage()

    if settings.printusage:
        searchoptions.usage()

    if settings.printversion:
        log('Version: 0.1')
        sys.exit(1)

    if settings.debug:
        log('settings: {0!s}'.format(settings))

    try:
        searcher = Searcher(settings)
        searcher.search()

        # print the results
        if settings.printresults:
            log('')
            searcher.print_results()

        if settings.listdirs:
            dir_list = searcher.get_matching_dirs()
            if dir_list:
                log('\nDirectories with matches (%d):' % len(dir_list))
                for d in dir_list:
                    log(d)

        if settings.listfiles:
            file_list = searcher.get_matching_files()
            if file_list:
                log('\nFiles with matches (%d):' % len(file_list))
                for f in file_list:
                    log(f)

        if settings.listlines:
            line_list = searcher.get_matching_lines()
            if line_list:
                log('\nLines with matches (%d):' % len(line_list))
                for line in line_list:
                    log(line)

    except AssertionError as e:
        log('ERROR: {0!s}\n'.format(e))
        searchoptions.usage()
    except KeyboardInterrupt:
        log('')
        sys.exit(0)

if __name__ == '__main__':
    main()
