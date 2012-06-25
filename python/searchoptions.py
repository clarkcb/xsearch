################################################################################
#
# searchoptions.py
#
# class SearchOptions: defines the available command-line options and
#                      corresponding utility methods
#
################################################################################
from cStringIO import StringIO
import os
import sys

from searchoption import SearchOption
from searchsettings import SearchSettings

class SearchOptions:
    arg_options = (
        SearchOption('b', 'numlinesbefore',
         lambda x, settings: settings.set_property('numlinesbefore', int(x)),
         'Number of lines to show before every matched line (default: 0)'),
        SearchOption('B', 'numlinesafter',
         lambda x, settings: settings.set_property('numlinesafter', int(x)),
         'Number of lines to show after every matched line (default: 0)'),
        SearchOption('d', 'dirname',
         lambda x, settings: settings.in_dirpatterns.add(x),
         'Specify name pattern for directories to include in search'),
        SearchOption('D', 'dirfilter',
         lambda x, settings: settings.out_dirpatterns.add(x),
         'Specify name pattern for directories to exclude from search'),
        SearchOption('f', 'filename',
         lambda x, settings: settings.in_filepatterns.add(x),
         'Specify name pattern for files to include in search'),
        SearchOption('F', 'filefilter',
         lambda x, settings: settings.out_filepatterns.add(x),
         'Specify name pattern for files to exclude from search'),
        SearchOption('', 'ignorecasesearchfile',
         lambda x, settings: settings.set_property('ignorecasesearchfile', x),
         'Specify file containing case-insensitive search patterns (one per line)'),
        SearchOption('', 'linesafterfilter',
         lambda x, settings: settings.linesafterfilters.append(x),
         'Specify pattern to filter the "lines-after" lines on (used with --numlinesafter)'),
        SearchOption('', 'linesaftersearch',
         lambda x, settings: settings.linesaftersearches.append(x),
         'Specify pattern to search the "lines-after" lines on (used with --numlinesafter)'),
        SearchOption('', 'linesbeforefilter',
         lambda x, settings: settings.linesbeforefilters.append(x),
         'Specify pattern to filter the "lines-before" lines on (used with --numlinesbefore)'),
        SearchOption('', 'linesbeforesearch',
         lambda x, settings: settings.linesbeforesearches.append(x),
         'Specify pattern to search the "lines-before" lines on (used with --numlinesbefore)'),
        SearchOption('s', 'search',
         lambda x, settings: settings.add_searchpattern(x),
         'Specify search pattern'),
        SearchOption('S', 'ignorecasesearch',
         lambda x, settings: settings.add_searchpattern(x, casesensitive=False),
         'Specify case-insensitive search pattern'),
        SearchOption('', 'searchfile',
         lambda x, settings: settings.set_property('searchfile', x),
         'Specify file containing search patterns (one per line)'),
        SearchOption('x', 'ext',
         lambda x, settings: settings.in_extensions.add(x),
         'Specify extension for files to include in search'),
        SearchOption('X', 'extfilter',
         lambda x, settings: settings.out_extensions.add(x),
         'Specify extension for files to exclude from search')
    )
    flag_options = (
        SearchOption('1', 'firstmatch',
         lambda settings: settings.set_property('firstmatch', True),
         'Capture only the first match for a file+search combination'),
        SearchOption('a', 'allmatches',
         lambda settings: settings.set_property('firstmatch', False),
         'Capture all matches*'),
        SearchOption('c', 'casesensitive',
         lambda settings: settings.set_property('ignorecase', False),
         'Do case-sensitive searching*'),
        SearchOption('C', 'caseinsensitive',
         lambda settings: settings.set_property('ignorecase', True),
         'Do case-insensitive searching'),
        SearchOption('', 'debug',
         lambda settings: settings.set_property('debug', True),
         'Set output mode to debug'),
        SearchOption('h', 'help',
         lambda settings: settings.set_property('printusage', True),
         'Print this usage and exit'),
        SearchOption('', 'listfiles',
         lambda settings: settings.set_property('listfiles', True),
         'Generate a list of the matching files after searching'),
        SearchOption('', 'listlines',
         lambda settings: settings.set_property('listlines', True),
         'Generate a list of the matching lines after searching'),
        SearchOption('m', 'multilinesearch',
         lambda settings: settings.set_property('multilinesearch', True),
         'Search files by line*'),
        SearchOption('p', 'printmatches',
         lambda settings: settings.set_property('printresults', True),
         'Print matches to stdout as found*'),
        SearchOption('P', 'noprintmatches',
         lambda settings: settings.set_property('printresults', False),
         'Suppress printing of matches to stdout'),
        SearchOption('t', 'dotiming',
         lambda settings: settings.set_property('dotiming', True),
         'Time search execution'),
        SearchOption('v', 'verbose',
         lambda settings: settings.set_property('verbose', True),
         'Specify verbose output'),
        SearchOption('V', 'version',
         lambda settings: settings.set_property('printversion', True),
         'Print the version and exit'),
        SearchOption('z', 'searchcompressed',
         lambda settings: settings.set_property('searchcompressed', True),
         'Search compressed files (bz2, gz, tar, zip)*'),
        SearchOption('Z', 'nosearchcompressed',
         lambda settings: settings.set_property('searchcompressed', False),
         'Do not search compressed files (bz2, gz, tar, zip)')
    )

    def __init__(self):
        self.options = self.arg_options + self.flag_options
        self.sorted_options = sorted(self.options, key=lambda opt: opt.sortarg)
        self.arg_dict = self.dict_from_options(self.arg_options)
        self.flag_dict = self.dict_from_options(self.flag_options)

    def dict_from_options(self, options):
        '''Returns a dict for a give collection of SearchOption objects'''
        opt_dict = {}
        for o in options:
            if o.shortarg:
                opt_dict[o.shortarg] = o
            opt_dict[o.longarg] = o
        return opt_dict

    def search_settings_from_args(self, args):
        settings = SearchSettings()
        while args:
            arg = args.pop(0)
            if arg.startswith('-'):
                while arg and arg.startswith('-'):
                    arg = arg[1:]
                if arg in self.arg_dict:
                    if args:
                        argval = args.pop(0)
                        self.arg_dict[arg].func(argval, settings)
                    else:
                        raise Exception('Missing value for option %s' % arg)
                elif arg in self.flag_dict:
                    self.flag_dict[arg].func(settings)
                    if arg in ('h', 'help', 'V', 'version'):
                        return settings
                else:
                    raise Exception('Unknown option: %s' % arg)
            else:
                settings.startpath = arg
        if not settings.startpath:
            raise Exception('Missing startpath')
        if not settings.searchpatterns:
            raise Exception('No search patterns specified')
        if settings.debug:
            settings.verbose = True
        return settings

    def get_usage_string(self):
        sio = StringIO()
        sio.write('Usage:\n')
        sio.write(' pysearch.py [options] <startpath>\n\nOptions:\n')
        opt_strings = []
        opt_descs = []
        longest = 0
        for opt in self.sorted_options:
            opt_string = ''
            if opt.shortarg:
                opt_string += '-%s,' % opt.shortarg
            opt_string += '--%s' % opt.longarg
            if len(opt_string) > longest:
                longest = len(opt_string)
            opt_strings.append(opt_string)
            opt_descs.append(opt.desc)
        format_string = ' %%-%ds  %%s\n' % longest
        for i, opt_string in enumerate(opt_strings):
            sio.write(format_string % (opt_string, opt_descs[i]))
        usage = sio.getvalue()
        sio.close()
        return usage
