# -*- coding: utf-8 -*-
###############################################################################
#
# searchoptions.py
#
# class SearchOptions: defines the available command-line options and
#                      corresponding utility methods
#
###############################################################################
import json
import os
import sys
from collections import deque
from io import StringIO
from typing import List

import pkg_resources

from .searchexception import SearchException
from .searchoption import SearchOption
from .searchsettings import SearchSettings


class SearchOptions(object):
    """class to provide usage info and parse command-line arguments into settings"""

    def __init__(self):
        self.options = []
        self.__set_dicts()
        self.__set_options_from_json()

    def __set_dicts(self):
        self.__bool_arg_dict = {
            'allmatches':
                lambda b, settings:
                settings.set_property('firstmatch', not b),
            'archivesonly':
                lambda b, settings:
                settings.set_property('archivesonly', b),
            'colorize':
                lambda b, settings:
                settings.set_property('colorize', b),
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
            'nocolorize':
                lambda b, settings:
                settings.set_property('colorize', not b),
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

        self.__coll_arg_dict = {
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
            'path':
                lambda x, settings:
                settings.paths.add(x),
            'searchpattern':
                lambda x, settings:
                settings.add_patterns(x, 'searchpatterns')
        }

        self.__int_arg_dict = {
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

        self.__str_arg_dict = {
            'encoding':
                lambda x, settings:
                settings.set_property('textfileencoding', x),
        }

        self.__longarg_dict = {}

    def settings_from_file(self, filepath: str, settings: SearchSettings):
        assert os.path.exists(filepath), \
            'Settings file not found: {0:s}'.format(filepath)
        with open(filepath) as f:
            jsonstr = f.read()
        self.settings_from_json(jsonstr, settings)

    def settings_from_json(self, jsonstr: str, settings: SearchSettings):
        json_dict = json.loads(jsonstr)
        for arg in json_dict:
            if arg in self.__bool_arg_dict:
                self.__bool_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__coll_arg_dict:
                self.__coll_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__int_arg_dict:
                self.__int_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__str_arg_dict:
                self.__str_arg_dict[arg](json_dict[arg], settings)
            else:
                raise SearchException('Invalid option: {0}'.format(arg))

    def __set_options_from_json(self):
        # with open(SEARCHOPTIONSPATH, mode='r') as f:
        #     searchoptions_dict = json.load(f)
        stream = pkg_resources.resource_stream(__name__, 'data/searchoptions.json')
        searchoptions_dict = json.load(stream)
        for searchoption_obj in searchoptions_dict['searchoptions']:
            longarg = searchoption_obj['long']
            shortarg = ''
            if 'short' in searchoption_obj:
                shortarg = searchoption_obj['short']
            desc = searchoption_obj['desc']
            if longarg in self.__bool_arg_dict:
                func = self.__bool_arg_dict[longarg]
            elif longarg in self.__coll_arg_dict:
                func = self.__coll_arg_dict[longarg]
            elif longarg in self.__int_arg_dict:
                func = self.__int_arg_dict[longarg]
            elif longarg in self.__str_arg_dict:
                func = self.__str_arg_dict[longarg]
            elif longarg in self.__str_arg_dict:
                func = self.__str_arg_dict[longarg]
            elif longarg == 'settings-file':
                func = self.settings_from_file
            else:
                raise SearchException(
                    'Unknown search option: {0:s}'.format(longarg))
            self.options.append(SearchOption(shortarg, longarg, desc, func))
            self.__longarg_dict[longarg] = longarg
            if shortarg:
                self.__longarg_dict[shortarg] = longarg

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
                if arg in self.__longarg_dict:
                    longarg = self.__longarg_dict[arg]
                    if longarg in self.__bool_arg_dict:
                        self.__bool_arg_dict[longarg](True, settings)
                        if longarg in ('help', 'version'):
                            return settings
                    elif longarg in self.__coll_arg_dict or \
                            longarg in self.__int_arg_dict or \
                            longarg in self.__str_arg_dict or \
                            longarg == 'settings-file':
                        if argdeque:
                            argval = argdeque.popleft()
                            if longarg in self.__coll_arg_dict:
                                self.__coll_arg_dict[longarg](argval, settings)
                            elif longarg in self.__int_arg_dict:
                                invalid_int = False
                                try:
                                    i = int(argval)
                                except ValueError:
                                    invalid_int = True
                                else:
                                    if i < 0:
                                        invalid_int = True
                                if invalid_int:
                                    err = 'Invalid value for option {}: {}'.format(
                                        arg, argval)
                                    raise SearchException(err)
                                self.__int_arg_dict[longarg](argval, settings)
                            elif longarg in self.__str_arg_dict:
                                self.__str_arg_dict[longarg](argval, settings)
                            elif longarg == 'settings-file':
                                self.settings_from_file(argval, settings)
                        else:
                            raise SearchException('Missing value for option {0}'.
                                                  format(arg))
                    else:
                        raise SearchException(
                            'Invalid option: {0}'.format(arg))
                else:
                    raise SearchException('Invalid option: {0}'.format(arg))
            else:
                settings.paths.add(arg)
        return settings

    def usage(self):
        print(self.__get_usage_string())
        sys.exit(1)

    def __get_usage_string(self):
        sio = StringIO()
        sio.write('Usage:\n')
        sio.write(
            ' pysearch [options] -s <searchpattern> <path> [<path> ...]\n\nOptions:\n')
        opt_pairs = []
        longest = 0
        for opt in sorted(self.options, key=lambda o: o.sortarg):
            opt_string = ''
            if opt.shortarg:
                opt_string += '-{0:s},'.format(opt.shortarg)
            opt_string += '--{0:s}'.format(opt.longarg)
            if len(opt_string) > longest:
                longest = len(opt_string)
            opt_pairs.append((opt_string, opt.desc))
        format_string = ' {0:<' + str(longest) + 's}  {1:s}\n'
        for opt_pair in opt_pairs:
            sio.write(format_string.format(opt_pair[0], opt_pair[1]))
        usage = sio.getvalue()
        sio.close()
        return usage
