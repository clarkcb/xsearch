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

def main():
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

    if settings.debug:
        print 'settings: {0!s}'.format(settings)

    try:
        searcher = Searcher(settings)
        searcher.search()

        # print the results
        if settings.printresults:
            print
            searcher.print_results()

        if settings.listdirs:
            dir_list = searcher.get_matching_dirs()
            if dir_list:
                print '\nDirectories with matches (%d):' % len(dir_list)
                for d in dir_list:
                    print d

        if settings.listfiles:
            file_list = searcher.get_matching_files()
            if file_list:
                print '\nFiles with matches (%d):' % len(file_list)
                for f in file_list:
                    print f

        if settings.listlines:
            line_list = searcher.get_matching_lines()
            if line_list:
                print '\nLines with matches (%d):' % len(line_list)
                for line in line_list:
                    print line

    except AssertionError as e:
        print 'ERROR: {0!s}\n'.format(e)
        searchoptions.usage()
    except KeyboardInterrupt:
        print
        sys.exit(0)

if __name__ == '__main__':
    main()
