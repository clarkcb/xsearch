#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
###############################################################################
#
# pysearch.py
#
# A CLI file search utility implemented in python (>=3.9.x)
#
###############################################################################
"""
import sys

from pyfind.common import log, log_error

from . import VERSION
from .searcher import (Searcher, print_search_results, print_search_dir_results,
                       print_search_file_results, print_search_lines_results)
from .searchexception import SearchException
from .searchoptions import SearchOptions
from .searchresult import SearchResultFormatter


async def main():
    if sys.version_info < (3, 9):
        sys.exit('Sorry, Python < 3.9 is not supported')

    search_options = SearchOptions()

    settings = None
    try:
        settings = search_options.search_settings_from_args(sys.argv[1:])
    except SearchException as e:
        log('')
        log_error(f'{e}\n')
        search_options.usage(1)

    if settings.debug:
        log(f'settings: {settings}')

    if settings.print_usage:
        log('')
        search_options.usage()

    if settings.print_version:
        log(f'xsearch version {VERSION}')
        sys.exit(0)

    try:
        searcher = Searcher(settings)
        results = await searcher.search()
        formatter = SearchResultFormatter(settings)

        # print the results
        if settings.print_results:
            log('')
            print_search_results(results, formatter)

        if settings.print_dirs:
            print_search_dir_results(results, formatter)

        if settings.print_files:
            print_search_file_results(results, formatter)

        if settings.print_lines:
            print_search_lines_results(results, formatter)

    except AssertionError as e:
        log('')
        log_error(f'{e}\n')
        search_options.usage(1)
    except KeyboardInterrupt:
        log('')
        sys.exit(0)


if __name__ == '__main__':
    main()
