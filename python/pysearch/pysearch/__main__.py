#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# pysearch.py
#
# A CLI file search utility implemented in python (3.x)
#
###############################################################################
import sys

from .common import log
from .config import VERSION
from .searcher import Searcher
from .searchexception import SearchException
from .searchoptions import SearchOptions


def main():
    searchoptions = SearchOptions()

    settings = None
    try:
        settings = searchoptions.search_settings_from_args(sys.argv[1:])
    except SearchException as e:
        log('\nERROR: {0!s}\n'.format(e))
        searchoptions.usage()

    if settings.debug:
        log('settings: {0!s}'.format(settings))

    if settings.printusage:
        log('')
        searchoptions.usage()

    if settings.printversion:
        log('xsearch version {}'.format(VERSION))
        sys.exit(0)

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
                msg = '\nLines with matches (%d):'
                if settings.uniquelines:
                    msg = '\nUnique lines with matches (%d):'
                log(msg % len(line_list))
                for line in line_list:
                    log(line)

    except AssertionError as e:
        log('\nERROR: {0!s}\n'.format(e))
        searchoptions.usage()
    except KeyboardInterrupt:
        log('')
        sys.exit(0)


if __name__ == '__main__':
    main()
