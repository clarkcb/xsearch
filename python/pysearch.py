#!/usr/bin/env python
################################################################################
#
# pysearch.py
#
# A file search utility implemented in python (2.x)
#
################################################################################
import sys

from searcher import Searcher
from searchoptions import SearchOptions

DEBUG = False

def main():
    if DEBUG:
        print 'sys.argv({0}): {1!s}'.format(len(sys.argv), sys.argv)

    searchoptions = SearchOptions()

    settings = None
    try:
        settings = searchoptions.search_settings_from_args(sys.argv[1:])
    except Exception, e:
        print '\nException: {0!s}\n'.format(e)
        searchoptions.usage()

    if settings.printusage:
        searchoptions.usage()

    if settings.printversion:
        print 'Version: 0.1'
        sys.exit(1)

    if DEBUG:
        settings.debug = True

    if settings.debug:
        print 'settings: {0!s}'.format(settings)

    try:
        searcher = Searcher(settings)
        searcher.search()
    except KeyboardInterrupt:
        print
        sys.exit(0)


if __name__ == '__main__':
    main()
