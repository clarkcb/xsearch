# -*- coding: utf-8 -*-
###############################################################################
#
# searchoptions.py
#
# class SearchOptions: defines the available command-line options and
#                      corresponding utility methods
#
###############################################################################
from collections import deque
from io import StringIO
import json
import os
import sys
from typing import List
import xml.dom.minidom as minidom

from .common import get_text
from .config import SEARCHOPTIONSPATH
from .searchexception import SearchException
from .searchoption import SearchOption
from .searchsettings import SearchSettings


class SearchOptions(object):
    """class to provide usage info and parse command-line arguments into settings"""

    def __init__(self):
        self.options = []
        self.set_dicts()
        self.set_options_from_json()

    def set_dicts(self):
        self.bool_arg_dict = {
            'allmatches':
                lambda b, settings:
                    settings.set_property('firstmatch', not b),
            'archivesonly':
                lambda b, settings:
                    settings.set_property('archivesonly', b),
            'debug':
                lambda b, settings:
                    settings.set_property('debug', b),
            'excludehidden':
                lambda b, settings:
                    settings.set_property('excludehidden', b),
            'firstmatch':
                lambda b, settings:
                    settings.set_property('firstmatch', b),
            'help':
                lambda b, settings:
                    settings.set_property('printusage', b),
            'includehidden':
                lambda b, settings:
                    settings.set_property('excludehidden', not b),
            'listdirs':
                lambda b, settings:
                    settings.set_property('listdirs', b),
            'listfiles':
                lambda b, settings:
                    settings.set_property('listfiles', b),
            'listlines':
                lambda b, settings:
                    settings.set_property('listlines', b),
            'multilinesearch':
                lambda b, settings:
                    settings.set_property('multilinesearch', b),
            'noprintmatches':
                lambda b, settings:
                    settings.set_property('printresults', not b),
            'norecursive':
                lambda b, settings:
                    settings.set_property('recursive', not b),
            'nosearcharchives':
                lambda b, settings:
                    settings.set_property('searcharchives', not b),
            'printmatches':
                lambda b, settings:
                    settings.set_property('printresults', b),
            'recursive':
                lambda b, settings:
                    settings.set_property('recursive', b),
            'searcharchives':
                lambda b, settings:
                    settings.set_property('searcharchives', b),
            'uniquelines':
                lambda b, settings:
                    settings.set_property('uniquelines', b),
            'verbose':
                lambda b, settings:
                    settings.set_property('verbose', b),
            'version':
                lambda b, settings:
                    settings.set_property('printversion', b)
        }

        self.coll_arg_dict = {
            'in-archiveext':
                lambda x, settings:
                    settings.add_exts(x, 'in_archiveextensions'),
            'in-archivefilepattern':
                lambda x, settings:
                    settings.add_patterns(x, 'in_archivefilepatterns'),
            'in-dirpattern':
                lambda x, settings:
                    settings.add_patterns(x, 'in_dirpatterns'),
            'in-ext':
                lambda x, settings:
                    settings.add_exts(x, 'in_extensions'),
            'in-filepattern':
                lambda x, settings:
                    settings.add_patterns(x, 'in_filepatterns'),
            'in-filetype':
                lambda x, settings:
                    settings.add_filetypes(x, 'in_filetypes'),
            'in-linesafterpattern':
                lambda x, settings:
                    settings.add_patterns(x, 'in_linesafterpatterns'),
            'in-linesbeforepattern':
                lambda x, settings:
                    settings.add_patterns(x, 'in_linesbeforepatterns'),
            'linesaftertopattern':
                lambda x, settings:
                    settings.add_patterns(x, 'linesaftertopatterns'),
            'linesafteruntilpattern':
                lambda x, settings:
                    settings.add_patterns(x, 'linesafteruntilpatterns'),
            'out-archiveext':
                lambda x, settings:
                    settings.add_exts(x, 'out_archiveextensions'),
            'out-archivefilepattern':
                lambda x, settings:
                    settings.add_patterns(x, 'out_archivefilepatterns'),
            'out-dirpattern':
                lambda x, settings:
                    settings.add_patterns(x, 'out_dirpatterns'),
            'out-ext':
                lambda x, settings:
                    settings.add_exts(x, 'out_extensions'),
            'out-filepattern':
                lambda x, settings:
                    settings.add_patterns(x, 'out_filepatterns'),
            'out-filetype':
                lambda x, settings:
                    settings.add_filetypes(x, 'out_filetypes'),
            'out-linesafterpattern':
                lambda x, settings:
                    settings.add_patterns(x, 'out_linesafterpatterns'),
            'out-linesbeforepattern':
                lambda x, settings:
                    settings.add_patterns(x, 'out_linesbeforepatterns'),
            'search':
                lambda x, settings:
                    settings.add_patterns(x, 'searchpatterns')
        }

        self.int_arg_dict = {
            'linesafter':
                lambda x, settings:
                    settings.set_property('linesafter', int(x)),
            'linesbefore':
                lambda x, settings:
                    settings.set_property('linesbefore', int(x)),
            'maxlinelength':
                lambda x, settings:
                    settings.set_property('maxlinelength', int(x)),
        }

        self.str_arg_dict = {
            'encoding':
                lambda x, settings:
                    settings.set_property('textfileencoding', x),
            'startpath':
                lambda x, settings:
                    settings.set_property('startpath', x),
        }

        self.longarg_dict = {}

    def settings_from_file(self, filepath: str, settings: SearchSettings):
        assert os.path.exists(filepath), 'Settings file not found: %s' % filepath
        with open(filepath) as f:
            jsonstr = f.read()
            # print "jsonstr: '%s'" % jsonstr
        self.settings_from_json(jsonstr, settings)

    def settings_from_json(self, jsonstr: str, settings: SearchSettings):
        json_dict = json.loads(jsonstr)
        for arg in json_dict:
            if arg in self.bool_arg_dict:
                self.bool_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.coll_arg_dict:
                self.coll_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.int_arg_dict:
                self.int_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.str_arg_dict:
                self.str_arg_dict[arg](json_dict[arg], settings)
            else:
                raise SearchException('Invalid option: {0}'.format(arg))

    def set_options_from_json(self):
        with open(SEARCHOPTIONSPATH, mode='r') as f:
            searchoptions_dict = json.load(f)
        for searchoption_obj in searchoptions_dict['searchoptions']:
            longarg = searchoption_obj['long']
            shortarg = ''
            if 'short' in searchoption_obj:
                shortarg = searchoption_obj['short']
            desc = searchoption_obj['desc']
            if longarg in self.bool_arg_dict:
                func = self.bool_arg_dict[longarg]
            elif longarg in self.coll_arg_dict:
                func = self.coll_arg_dict[longarg]
            elif longarg in self.int_arg_dict:
                func = self.int_arg_dict[longarg]
            elif longarg in self.str_arg_dict:
                func = self.str_arg_dict[longarg]
            elif longarg in self.str_arg_dict:
                func = self.str_arg_dict[longarg]
            elif longarg == 'settings-file':
                func = self.settings_from_file
            else:
                raise SearchException('Unknown search option: %s' % longarg)
            self.options.append(SearchOption(shortarg, longarg, desc, func))
            self.longarg_dict[longarg] = longarg
            if shortarg:
                self.longarg_dict[shortarg] = longarg

    def set_options_from_xml(self):
        searchoptionsdom = minidom.parse(SEARCHOPTIONSPATH)
        searchoptionnodes = searchoptionsdom.getElementsByTagName('searchoption')
        for searchoptionnode in searchoptionnodes:
            longarg = searchoptionnode.getAttribute('long')
            shortarg = searchoptionnode.getAttribute('short')
            desc = get_text(searchoptionnode.childNodes).strip()
            if longarg in self.bool_arg_dict:
                func = self.bool_arg_dict[longarg]
            elif longarg in self.coll_arg_dict:
                func = self.coll_arg_dict[longarg]
            elif longarg in self.int_arg_dict:
                func = self.int_arg_dict[longarg]
            elif longarg in self.str_arg_dict:
                func = self.str_arg_dict[longarg]
            elif longarg == 'settings-file':
                func = self.settings_from_file
            else:
                raise SearchException('Unknown search option: %s' % longarg)
            self.options.append(SearchOption(shortarg, longarg, desc, func))
            self.longarg_dict[longarg] = longarg
            if shortarg:
                self.longarg_dict[shortarg] = longarg

    def search_settings_from_args(self, args: List[str]) -> SearchSettings:
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
                if arg in self.longarg_dict:
                    longarg = self.longarg_dict[arg]
                    if longarg in self.bool_arg_dict:
                        self.bool_arg_dict[longarg](True, settings)
                        if longarg in ('help', 'version'):
                            return settings
                    elif longarg in self.coll_arg_dict or \
                            longarg in self.int_arg_dict or \
                            longarg in self.str_arg_dict or \
                            longarg == 'settings-file':
                        if argdeque:
                            argval = argdeque.popleft()
                            if longarg in self.coll_arg_dict:
                                self.coll_arg_dict[longarg](argval, settings)
                            elif longarg in self.int_arg_dict:
                                invalid_int = False
                                try:
                                    i = int(argval)
                                except ValueError:
                                    invalid_int = True
                                else:
                                    if i < 0:
                                        invalid_int = True
                                if invalid_int:
                                    err = 'Invalid value for option {}: {}'.format(arg, argval)
                                    raise SearchException(err)
                                self.int_arg_dict[longarg](argval, settings)
                            elif longarg in self.str_arg_dict:
                                self.str_arg_dict[longarg](argval, settings)
                            elif longarg == 'settings-file':
                                self.settings_from_file(argval, settings)
                        else:
                            raise SearchException('Missing value for option {0}'.
                                            format(arg))
                    else:
                        raise SearchException('Invalid option: {0}'.format(arg))
                else:
                    raise SearchException('Invalid option: {0}'.format(arg))
            else:
                settings.startpath = arg
        return settings

    def usage(self):
        print(self.get_usage_string())
        sys.exit(1)

    def get_usage_string(self):
        sio = StringIO()
        sio.write('Usage:\n')
        sio.write(' pysearch.py [options] -s <searchpattern> <startpath>\n\nOptions:\n')
        opt_strings = []
        opt_descs = []
        longest = 0
        for opt in sorted(self.options, key=lambda o: o.sortarg):
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
