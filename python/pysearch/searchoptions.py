# -*- coding: utf-8 -*-
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
import xml.dom.minidom as minidom

from searchoption import SearchOption
from searchsettings import SearchSettings

class SearchOptions(object):
    """class to provide usage info and parse command-line arguments into settings"""

    # TODO: move to a config file
    SEARCHOPTIONSPATH = '~/src/git/xsearch/shared/searchoptions.xml'

    arg_action_dict = {
        'in-archiveext':
            lambda x, settings:
                settings.add_comma_delimited_exts(x, 'in_archiveextensions'),
        'in-archivefilepattern':
            lambda x, settings:
                settings.add_pattern(x, 'in_archivefilepatterns'),
        'in-dirpattern':
            lambda x, settings:
                settings.add_pattern(x, 'in_dirpatterns'),
        'in-ext':
            lambda x, settings:
                settings.add_comma_delimited_exts(x, 'in_extensions'),
        'in-filepattern':
            lambda x, settings:
                settings.add_pattern(x, 'in_filepatterns'),
        'in-linesafterpattern':
            lambda x, settings:
                settings.add_pattern(x, 'in_linesafterpatterns'),
        'in-linesbeforepattern':
            lambda x, settings:
                settings.add_pattern(x, 'in_linesbeforepatterns'),
        'linesafter':
            lambda x, settings:
                settings.set_property('linesafter', int(x)),
        'linesaftertopattern':
            lambda x, settings:
                settings.add_pattern(x, 'linesaftertopatterns'),
        'linesafteruntilpattern':
            lambda x, settings:
                settings.add_pattern(x, 'linesafteruntilpatterns'),
        'linesbefore':
            lambda x, settings:
                settings.set_property('linesbefore', int(x)),
        'maxlinelength':
            lambda x, settings:
                settings.set_property('maxlinelength', int(x)),
        'out-archiveext':
            lambda x, settings:
                settings.add_comma_delimited_exts(x, 'out_archiveextensions'),
        'out-archivefilepattern':
            lambda x, settings:
                settings.add_pattern(x, 'out_archivefilepatterns'),
        'out-dirpattern':
            lambda x, settings:
                settings.add_pattern(x, 'out_dirpatterns'),
        'out-ext':
            lambda x, settings:
                settings.add_comma_delimited_exts(x, 'out_extensions'),
        'out-filepattern':
            lambda x, settings:
                settings.add_pattern(x, 'out_filepatterns'),
        'out-linesafterpattern':
            lambda x, settings:
                settings.add_pattern(x, 'out_linesafterpatterns'),
        'out-linesbeforepattern':
            lambda x, settings:
                settings.add_pattern(x, 'out_linesbeforepatterns'),
        'search':
            lambda x, settings:
                settings.add_pattern(x, 'searchpatterns')
    }
    flag_action_dict = {
        'allmatches':
            lambda settings:
                settings.set_property('firstmatch', False),
        'archivesonly':
            lambda settings:
                settings.set_properties({'archivesonly': True,
                                         'searcharchives': True}),
        'debug':
            lambda settings:
                settings.set_properties({'debug': True,
                                         'verbose': True}),
        'dotiming':
            lambda settings:
                settings.set_property('dotiming', True),
        'excludehidden':
            lambda settings:
                settings.set_property('excludehidden', True),
        'firstmatch':
            lambda settings:
                settings.set_property('firstmatch', True),
        'help':
            lambda settings:
                settings.set_property('printusage', True),
        'includehidden':
            lambda settings:
                settings.set_property('excludehidden', False),
        'listdirs':
            lambda settings:
                settings.set_property('listdirs', True),
        'listfiles':
            lambda settings:
                settings.set_property('listfiles', True),
        'listlines':
            lambda settings:
                settings.set_property('listlines', True),
        'multilinesearch':
            lambda settings:
                settings.set_property('multilinesearch', True),
        'noprintmatches':
            lambda settings:
                settings.set_property('printresults', False),
        'norecursive':
            lambda settings:
                settings.set_property('recursive', False),
        'nosearcharchives':
            lambda settings:
                settings.set_property('searcharchives', False),
        'printmatches':
            lambda settings:
                settings.set_property('printresults', True),
        'recursive':
            lambda settings:
                settings.set_property('recursive', True),
        'searcharchives':
            lambda settings:
                settings.set_property('searcharchives', True),
        'uniquelines':
            lambda settings:
                settings.set_property('uniquelines', True),
        'verbose':
            lambda settings:
                settings.set_property('verbose', True),
        'version':
            lambda settings:
                settings.set_property('printversion', True)
    }

    def __init__(self):
        self.options = []
        self.arg_dict = {}
        self.flag_dict = {}
        self.set_options_from_xml()
        self.sorted_options = sorted(self.options, key=lambda opt: opt.sortarg)

    def get_text(self, nodelist):
        rc = []
        for node in nodelist:
            if node.nodeType == node.TEXT_NODE:
                rc.append(node.data)
        return ''.join(rc)

    def set_options_from_xml(self):
        searchoptionsdom = minidom.parse(os.path.expanduser(self.SEARCHOPTIONSPATH))
        searchoptionnodes = searchoptionsdom.getElementsByTagName('searchoption')
        for searchoptionnode in searchoptionnodes:
            name = searchoptionnode.getAttribute('long')
            short = searchoptionnode.getAttribute('short')
            desc = self.get_text(searchoptionnode.childNodes).strip()
            func = None
            if name in self.arg_action_dict:
                func = self.arg_action_dict[name]
            elif name in self.flag_action_dict:
                func = self.flag_action_dict[name]
            else:
                raise ValueError('Unknown search option: %s' % name)
            option = SearchOption(short, name, desc, func)
            self.options.append(option)
            if name in self.arg_action_dict:
                self.arg_dict[name] = option
                if short:
                    self.arg_dict[short] = option
            elif name in self.flag_action_dict:
                self.flag_dict[name] = option
                if short:
                    self.flag_dict[short] = option

    def search_settings_from_args(self, args):
        """Returns a SearchSettings instance for a given list of args"""
        settings = SearchSettings()
        # default printresults to True since running from command line
        settings.printresults = True
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
