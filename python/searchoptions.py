################################################################################
#
# searchoptions.py
#
# class SearchOptions: defines the available command-line options and
#                      corresponding utility methods
#
################################################################################
from collections import deque
from cStringIO import StringIO
import os
import sys

from searchoption import SearchOption
from searchsettings import SearchSettings

class SearchOptions:
    arg_options = (
        SearchOption('b', 'linesbefore',
            'Number of lines to show before every matched line (default: 0)',
            lambda x, settings:
                settings.set_property('numlinesbefore', int(x))),
        SearchOption('B', 'linesafter',
            'Number of lines to show after every matched line (default: 0)',
            lambda x, settings:
                settings.set_property('numlinesafter', int(x))),
        SearchOption('d', 'in-dirpattern',
            'Specify name pattern for directories to include in search',
            lambda x, settings:
                settings.add_pattern(x, 'in_dirpatterns')),
        SearchOption('D', 'out-dirpattern',
            'Specify name pattern for directories to exclude from search',
            lambda x, settings:
                settings.add_pattern(x, 'out_dirpatterns')),
        SearchOption('f', 'in-filepattern',
            'Specify name pattern for files to include in search',
            lambda x, settings:
                settings.add_pattern(x, 'in_filepatterns')),
        SearchOption('F', 'out-filepattern',
            'Specify name pattern for files to exclude from search',
            lambda x, settings:
                settings.add_pattern(x, 'out_filepatterns')),
        SearchOption('', 'out-linesafterpattern',
            'Specify pattern to filter the "lines-after" lines on (used with' +
            ' --linesafter)',
            lambda x, settings:
                settings.add_pattern(x, 'out_linesafterpatterns')),
        SearchOption('', 'in-linesafterpattern',
            'Specify pattern to search the "lines-after" lines on (used with' +
            ' --linesafter)',
            lambda x, settings:
                settings.add_pattern(x, 'in_linesafterpatterns')),
        SearchOption('', 'out-linesbeforepattern',
            'Specify pattern to filter the "lines-before" lines on (used with' +
            ' --linesbefore)',
            lambda x, settings:
                settings.add_pattern(x, 'out_linesbeforepatterns')),
        SearchOption('', 'in-linesbeforepattern',
            'Specify pattern to search the "lines-before" lines on (used with' +
            ' --linesbefore)',
            lambda x, settings:
                settings.add_pattern(x, 'in_linesbeforepatterns')),
        SearchOption('s', 'search',
            'Specify search pattern',
            lambda x, settings:
                settings.add_pattern(x, 'searchpatterns')),
        SearchOption('x', 'ext',
            'Specify extension for files to include in search',
            lambda x, settings:
                settings.in_extensions.add(x)),
        SearchOption('X', 'extfilter',
            'Specify extension for files to exclude from search',
            lambda x, settings:
                settings.out_extensions.add(x))
    )
    flag_options = (
        SearchOption('1', 'firstmatch',
            'Capture only the first match for a file+search combination',
            lambda settings:
                settings.set_property('firstmatch', True)),
        SearchOption('a', 'allmatches',
            'Capture all matches*',
            lambda settings:
                settings.set_property('firstmatch', False)),
        SearchOption('', 'debug',
            'Set output mode to debug',
            lambda settings:
                settings.set_property('debug', True)),
        SearchOption('h', 'help',
            'Print this usage and exit',
            lambda settings:
                settings.set_property('printusage', True)),
        SearchOption('', 'listfiles',
            'Generate a list of the matching files after searching',
            lambda settings:
                settings.set_property('listfiles', True)),
        SearchOption('', 'listlines',
            'Generate a list of the matching lines after searching',
            lambda settings:
                settings.set_property('listlines', True)),
        SearchOption('m', 'multilinesearch',
            'Search files by line*',
            lambda settings:
                settings.set_property('multilinesearch', True)),
        SearchOption('p', 'printmatches',
            'Print matches to stdout as found*',
            lambda settings:
                settings.set_property('printresults', True)),
        SearchOption('P', 'noprintmatches',
            'Suppress printing of matches to stdout',
            lambda settings:
                settings.set_property('printresults', False)),
        SearchOption('t', 'dotiming',
            'Time search execution',
            lambda settings:
                settings.set_property('dotiming', True)),
        SearchOption('v', 'verbose',
            'Set output mode to verbose',
            lambda settings:
                settings.set_property('verbose', True)),
        SearchOption('V', 'version',
            'Print version and exit',
            lambda settings:
                settings.set_property('printversion', True)),
        SearchOption('z', 'searchcompressed',
            'Search compressed files (bz2, gz, tar, zip)*',
            lambda settings:
                settings.set_property('searchcompressed', True)),
        SearchOption('Z', 'nosearchcompressed',
            'Do not search compressed files (bz2, gz, tar, zip)',
            lambda settings:
                settings.set_property('searchcompressed', False))
    )

    def __init__(self):
        self.options = self.arg_options + self.flag_options
        self.sorted_options = sorted(self.options, key=lambda opt: opt.sortarg)
        self.arg_dict = self.dict_from_options(self.arg_options)
        self.flag_dict = self.dict_from_options(self.flag_options)

    def dict_from_options(self, options):
        """Returns a dict for a given collection of SearchOption objects"""
        opt_dict = {}
        for o in options:
            if o.shortarg:
                opt_dict[o.shortarg] = o
            opt_dict[o.longarg] = o
        return opt_dict

    def search_settings_from_args(self, args):
        """Returns a SearchSettings instance for a given list of args"""
        settings = SearchSettings()
        argdeque = deque(args)
        while argdeque:
            arg = argdeque.popleft()
            if arg.startswith('-'):
                while arg and arg.startswith('-'):
                    arg = arg[1:]
                if arg in self.arg_dict:
                    if argdeque:
                        argval = argdeque.popleft()
                        self.arg_dict[arg].func(argval, settings)
                    else:
                        raise Exception('Missing value for option {0}'.
                            format(arg))
                elif arg in self.flag_dict:
                    self.flag_dict[arg].func(settings)
                    if arg in ('h', 'help', 'V', 'version'):
                        return settings
                else:
                    raise Exception('Unknown option: {0}'.format(arg))
            else:
                settings.startpath = arg
        if settings.debug:
            settings.verbose = True
        return settings

    def usage(self):
        print self.get_usage_string()
        sys.exit(1)

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
                opt_string += '-{0},'.format(opt.shortarg)
            opt_string += '--{0}'.format(opt.longarg)
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
