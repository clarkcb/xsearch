#!/usr/bin/env python
################################################################################
#
# pysearch.py
#
# A file search utility implemented in python (2.x)
#
################################################################################

__version__ = "$Revision: $"
# $Source$

from cStringIO import StringIO
import sys

from searcher import Searcher
from searchoption import SearchOption
from searchsettings import SearchSettings

TARFILE_MODULE_AVAILABLE = True
ZIPFILE_MODULE_AVAILABLE = True

try:
    import tarfile
except ImportError, e:
    print 'tarfile not imported: %s' % e
    TARFILE_MODULE_AVAILABLE = False
try:
    import zipfile
except ImportError, e:
    print 'zipfile not imported: %s' % e
    ZIPFILE_MODULE_AVAILABLE = False

DEBUG = False

arg_options = (
    SearchOption('b', 'numlinesbefore', lambda x, settings: settings.set_property('numlinesbefore', int(x)),
     'Number of lines to show before every match (default is 0)'),
    SearchOption('B', 'numlinesafter', lambda x, settings: settings.set_property('numlinesafter', int(x)),
     'Number of lines to show after every match (default is 0)'),
    SearchOption('d', 'dirname', lambda x, settings: settings.in_dirpatterns.add(x),
     'Specify name pattern for directories to include in search'),
    SearchOption('D', 'dirfilter', lambda x, settings: settings.out_dirpatterns.add(x),
     'Specify name pattern for directories to exclude from search'),
    SearchOption('f', 'filename', lambda x, settings: settings.in_filepatterns.add(x),
     'Specify name pattern for files to include in search'),
    SearchOption('F', 'filefilter', lambda x, settings: settings.out_filepatterns.add(x),
     'Specify name pattern for files to exclude from search'),
    SearchOption('', 'ignorecasesearchfile', lambda x, settings: settings.set_property('ignorecasesearchfile', x),
     'Specify file containing case-insensitive search patterns (one per line)'),
    SearchOption('', 'linesafterfilter', lambda x, settings: settings.linesafterfilters.append(x),
     'Specify pattern to filter the "lines-after" lines on (used with --numlinesafter)'),
    SearchOption('', 'linesaftersearch', lambda x, settings: settings.linesaftersearches.append(x),
     'Specify pattern to search the "lines-after" lines on (used with --numlinesafter)'),
    SearchOption('', 'linesbeforefilter', lambda x, settings: settings.linesbeforefilters.append(x),
     'Specify pattern to filter the "lines-before" lines on (used with --numlinesbefore)'),
    SearchOption('', 'linesbeforesearch', lambda x, settings: settings.linesbeforesearches.append(x),
     'Specify pattern to search the "lines-before" lines on (used with --numlinesbefore)'),
    SearchOption('s', 'search', lambda x, settings: settings.add_searchpattern(x),
     'Specify search pattern'),
    SearchOption('S', 'ignorecasesearch', lambda x, settings: settings.add_searchpattern(x, casesensitive=False),
     'Specify case-insensitive search pattern'),
    SearchOption('', 'searchfile', lambda x, settings: settings.set_property('searchfile', x),
     'Specify file containing search patterns (one per line)'),
    SearchOption('x', 'ext', lambda x, settings: settings.in_extensions.add(x),
     'Specify extension for files to include in search'),
    SearchOption('X', 'extfilter', lambda x, settings: settings.out_extensions.add(x),
     'Specify extension for files to exclude from search')
)
flag_options = (
    SearchOption('1', 'firstmatch', lambda settings: settings.set_property('firstmatch', True),
     'Capture only the first match for a file+search combination'),
    SearchOption('a', 'allmatches', lambda settings: settings.set_property('firstmatch', False),
     'Capture all matches*'),
    SearchOption('c', 'casesensitive', lambda settings: settings.set_property('ignorecase', False),
     'Do case-sensitive searching*'),
    SearchOption('C', 'ignorecase', lambda settings: settings.set_property('ignorecase', True),
     'Do case-insensitive searching'),
    SearchOption('', 'debug', lambda settings: settings.set_property('debug', True),
     'Set output mode to debug'),
    SearchOption('', 'listfiles', lambda settings: settings.set_property('listfiles', True),
     'Generate a list of the matching files after searching'),
    SearchOption('', 'listlines', lambda settings: settings.set_property('listlines', True),
     'Generate a list of the matching lines after searching'),
    SearchOption('m', 'multilinesearch', lambda settings: settings.set_property('multilinesearch', True),
     'Search files by line*'),
    SearchOption('p', 'printmatches', lambda settings: settings.set_property('printresults', True),
     'Print matches to stdout as found*'),
    SearchOption('P', 'noprintmatches', lambda settings: settings.set_property('printresults', False),
     'Suppress printing of matches to stdout'),
    SearchOption('t', 'dotiming', lambda settings: settings.set_property('dotiming', True),
     'Time search execution'),
    SearchOption('v', 'verbose', lambda settings: settings.set_property('verbose', True),
     'Specify verbose output'),
    SearchOption('z', 'searchcompressed', lambda settings: settings.set_property('searchcompressed', True),
     'Search compressed files (bz2, gz, tar, zip)*'),
    SearchOption('Z', 'nosearchcompressed', lambda settings: settings.set_property('searchcompressed', False),
     'Search compressed files (bz2, gz, tar, zip)')
)

def get_arg_maps():
    '''Returns arg_map and flag_map'''
    arg_dict = {}
    for o in arg_options:
        if o.shortarg:
            arg_dict[o.shortarg] = o
        arg_dict[o.longarg] = o
    flag_dict = {}
    for o in flag_options:
         if o.shortarg:
             flag_dict[o.shortarg] = o
         flag_dict[o.longarg] = o
    return arg_dict, flag_dict


def search_settings_from_args(args):
    arg_dict, flag_dict = get_arg_maps()
    settings = SearchSettings()

    while args:
        arg = args.pop(0)
        if False:
            print 'next arg: "%s"' % arg
        if arg.startswith('-'):
            while arg and arg.startswith('-'):
                arg = arg[1:]
            if arg in arg_dict:
                if False:
                    print '"%s" found in arg_dict' % arg
                if args:
                    argval = args.pop(0)
                    arg_dict[arg].func(argval, settings)
                else:
                    print 'Error: missing value for option %s' % arg
                    usage()
            elif arg in flag_dict:
                if False:
                    print '"%s" found in flag_dict' % arg
                flag_dict[arg].func(settings)
            elif arg in ('V', 'version'):
                print __version__
                sys.exit(0)
            else:
                print 'Error: unknown option: %s' % arg
        else:
            settings.startpath = arg

    if not settings.startpath:
        print 'Error: missing startpath'
        usage()

    return settings

def get_option_sort_elem(option):
    if option.shortarg:
        return option.shortarg.lower()
    else:
        return option.longarg.lower()

def usage():
    print 'Usage:'
    print '%s [options] <startpath>\n\noptions:' % os.path.basename(sys.argv[0])
    options = arg_options + flag_options
    opt_strings = []
    opt_descs = []
    longest = 0
    for opt in sorted(options, key=lambda opt: get_option_sort_elem(opt)):
        s = ''
        if opt.shortarg:
            s += '-%s,' % opt.shortarg
        s += '--%s' % opt.longarg
        if len(s) > longest:
            longest = len(s)
        opt_strings.append(s)
        opt_descs.append(opt.desc)
    format_string = ' %%-%ds  %%s' % longest
    for i,s in enumerate(opt_strings):
        print format_string % (s, opt_descs[i])
    sys.exit(1)


def main():
    if DEBUG:
        print 'sys.argv(%d): %s' % (len(sys.argv),str(sys.argv))

    if len(sys.argv) < 4:
        usage()

    settings = search_settings_from_args(sys.argv[1:])

    if DEBUG:
        print 'searchsettings: %s' % str(settings)

    try:
        searcher = Searcher(settings)
        searcher.search()
    except KeyboardInterrupt:
        print
        sys.exit(0)

    print
    searcher.print_res_counts()

    if settings.listfiles:
        file_list = searcher.get_file_list()
        if file_list:
            print '\nFiles with matches:'
            for f in file_list:
                print f

    if settings.listlines:
        line_list = searcher.get_line_list()
        if line_list:
            print '\nLines with matches:'
            for line in line_list:
                print line


if __name__ == '__main__':
    main()
