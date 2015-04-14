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

import common
from searcher import Searcher
from searchoptions import SearchOptions

def main():
    searchoptions = SearchOptions()

    settings = None
    try:
        settings = searchoptions.search_settings_from_args(sys.argv[1:])
    except Exception, e:
        common.log('\nException: {0!s}\n'.format(e))
        searchoptions.usage()

    if settings.printusage:
        searchoptions.usage()

    if settings.printversion:
        common.log('Version: 0.1')
        sys.exit(1)

    if settings.debug:
        common.log('settings: {0!s}'.format(settings))

    try:
        searcher = Searcher(settings)
        searcher.search()

        # print the results
        if settings.printresults:
            common.log('')
            searcher.print_results()

        if settings.listdirs:
            dir_list = searcher.get_matching_dirs()
            if dir_list:
                common.log('\nDirectories with matches (%d):' % len(dir_list))
                for d in dir_list:
                    common.log(d)

        if settings.listfiles:
            file_list = searcher.get_matching_files()
            if file_list:
                common.log('\nFiles with matches (%d):' % len(file_list))
                for f in file_list:
                    common.log(f)

        if settings.listlines:
            line_list = searcher.get_matching_lines()
            if line_list:
                msg = '\nLines with matches (%d):'
                if settings.uniquelines:
                    msg = '\nUnique lines with matches (%d):'
                common.log(msg % len(line_list))
                for line in line_list:
                    common.log(line)

    except AssertionError as e:
        common.log('\nERROR: {0!s}\n'.format(e))
        searchoptions.usage()
    except KeyboardInterrupt:
        common.log('')
        sys.exit(0)

if __name__ == '__main__':
    main()
