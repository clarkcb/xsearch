#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# pysearch.py
#
# A CLI file search utility implemented in python (3.x)
#
###############################################################################
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
    return sorted(results, key=lambda r: r.sortkey)


def print_results(results: List[SearchResult], settings: SearchSettings):
    sorted_results = get_sorted_results(results)
    formatter = SearchResultFormatter(settings)
    log('Search results ({}):'.format(len(sorted_results)))
    for r in sorted_results:
        s = formatter.format(r)
        try:
            log(s)
        except UnicodeEncodeError:
            log(repr(s))


def get_matching_dirs(results: List[SearchResult]) -> List[str]:
    """Get list of dirs with matches"""
    dirs = set([r.file.path for r in results])
    dirs = list(dirs)
    dirs.sort()
    return dirs


def get_matching_files(results: List[SearchResult]) -> List[str]:
    """Get list of files with matches"""
    files = set([(r.file.path, r.file.filename) for r in results])
    files = list(files)
    files.sort()
    return [os.path.join(f[0], f[1]) for f in files]


def get_matching_lines(results: List[SearchResult], settings: SearchSettings) -> List[str]:
    """Get list of lines with matches (unique if settings.uniquelines)"""
    lines = [r.line for r in results if r.line]
    if settings.uniquelines:
        lines = list(set(lines))
    return sorted(lines, key=lambda s: s.upper())


async def main():
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
        results = await searcher.search()

        # print the results
        if settings.printresults:
            log('')
            print_results(results, settings)

        if settings.listdirs:
            dirs = get_matching_dirs(results)
            if dirs:
                log('\nDirectories with matches ({}):'.format(len(dirs)))
                for d in dirs:
                    log(d)

        if settings.listfiles:
            files = get_matching_files(results)
            if files:
                log('\nFiles with matches ({}):'.format(len(files)))
                for f in files:
                    log(f)

        if settings.listlines:
            lines = get_matching_lines(results, settings)
            if lines:
                msg = '\nLines with matches ({}):'
                if settings.uniquelines:
                    msg = '\nUnique lines with matches ({}):'
                log(msg.format(len(lines)))
                for line in lines:
                    log(line)

    except AssertionError as e:
        log('\nERROR: {0!s}\n'.format(e))
        searchoptions.usage()
    except KeyboardInterrupt:
        log('')
        sys.exit(0)


if __name__ == '__main__':
    main()
