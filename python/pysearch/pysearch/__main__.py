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

from pyfind.common import log, log_error

from .searcher import Searcher
from .searchexception import SearchException
from .searchoptions import SearchOptions
from .searchresult import SearchResult, SearchResultFormatter
from .searchsettings import SearchSettings

VERSION = '0.1.0'

def get_sorted_results(results: List[SearchResult]):
    return sorted(results, key=lambda r: r.sort_key)


def print_results(results: List[SearchResult], settings: SearchSettings):
    formatter = SearchResultFormatter(settings)
    log(f'Search results ({len(results)}):')
    for r in results:
        s = formatter.format(r)
        try:
            log(s)
        except UnicodeEncodeError:
            log(repr(s))


def get_matching_dirs(search_results: List[SearchResult]) -> List[str]:
    """Return unique list of directories from file results"""
    return sorted(list({
        str(r.file.path.parent)
        for r in search_results
        if r.file and r.file.path and r.file.path.parent
    }))


def get_matching_files(search_results: List[SearchResult]) -> List[str]:
    """Get list of files with matches"""
    file_set = {}
    files = []
    for r in search_results:
        if r.file:
            if str(r.file) not in file_set:
                files.append(str(r.file))
                file_set[str(r.file)] = True
    return files


def get_matching_lines(search_results: List[SearchResult], settings: SearchSettings) -> List[str]:
    """Get list of lines with matches (unique if settings.unique_lines)"""
    lines = [r.line.lstrip() for r in search_results if r.line]
    if settings.unique_lines:
        lines = list(set(lines))
    return list(sorted(lines, key=lambda s: s.upper()))


async def main():
    search_options = SearchOptions()

    settings = None
    try:
        settings = search_options.search_settings_from_args(sys.argv[1:])
    except SearchException as e:
        log_error(str(e))
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

        if settings.print_dirs:
            dirs = get_matching_dirs(results)
            if dirs:
                log(f'\nMatching directories ({len(dirs)}):')
                for d in dirs:
                    log(d)
            else:
                log('\nMatching directories: 0')

        if settings.print_files:
            files = get_matching_files(results)
            if files:
                log(f'\nMatching files ({len(files)}):')
                for f in files:
                    log(f)
            else:
                log('\nMatching files: 0')

        if settings.print_lines:
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
        log_error(str(e))
        search_options.usage(1)
    except KeyboardInterrupt:
        log('')
        sys.exit(0)


if __name__ == '__main__':
    main()
