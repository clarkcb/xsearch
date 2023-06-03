#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
###############################################################################
#
# pysearch.py
#
# A CLI file search utility implemented in python (3.x)
#
###############################################################################
"""
import os
import sys
from typing import List

from .common import log
from .config import VERSION
from .searcher import Searcher
from .searchexception import SearchException
from .searchoptions import SearchOptions
from .searchresult import SearchResult, SearchResultFormatter
from .searchsettings import SearchSettings


def get_sorted_results(results: List[SearchResult]):
    return sorted(results, key=lambda r: r.sort_key)


def print_results(results: List[SearchResult], settings: SearchSettings):
    sorted_results = get_sorted_results(results)
    formatter = SearchResultFormatter(settings)
    log(f'Search results ({len(sorted_results)}):')
    for r in sorted_results:
        s = formatter.format(r)
        try:
            log(s)
        except UnicodeEncodeError:
            log(repr(s))


def get_matching_dirs(results: List[SearchResult]) -> List[str]:
    """Get list of dirs with matches"""
    dirs = set([r.file.path for r in results])
    dirs = sorted(dirs)
    return dirs


def get_matching_files(results: List[SearchResult]) -> List[str]:
    """Get list of files with matches"""
    files = set([(r.file.path, r.file.file_name) for r in results])
    files = sorted(files)
    return [os.path.join(f[0], f[1]) for f in files]


def get_matching_lines(
        results: List[SearchResult], settings: SearchSettings) -> List[str]:
    """Get list of lines with matches (unique if settings.unique_lines)"""
    lines = [r.line for r in results if r.line]
    if settings.unique_lines:
        lines = list(set(lines))
    return sorted(lines, key=lambda s: s.upper())


async def main():
    search_options = SearchOptions()

    settings = None
    try:
        settings = search_options.search_settings_from_args(sys.argv[1:])
    except SearchException as e:
        log(f'\nERROR: {e}\n')
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

        # print the results
        if settings.print_results:
            log('')
            print_results(results, settings)

        if settings.list_dirs:
            dirs = get_matching_dirs(results)
            if dirs:
                log(f'\nMatching directories ({len(dirs)}):')
                for d in dirs:
                    log(d)
            else:
                log('\nMatching directories: 0')

        if settings.list_files:
            files = get_matching_files(results)
            if files:
                log(f'\nMatching files ({len(files)}):')
                for f in files:
                    log(f)
            else:
                log('\nMatching files: 0')

        if settings.list_lines:
            lines = get_matching_lines(results, settings)
            if lines:
                line_len = len(lines)
                if settings.unique_lines:
                    msg = f'\nUnique matching lines ({line_len}):'
                else:
                    msg = f'\nMatching lines ({line_len}):'
                log(msg)
                for line in lines:
                    log(line)
            else:
                log('\nMatching lines: 0')

    except AssertionError as e:
        log(f'\nERROR: {e}\n')
        search_options.usage(1)
    except KeyboardInterrupt:
        log('')
        sys.exit(0)


if __name__ == '__main__':
    main()
