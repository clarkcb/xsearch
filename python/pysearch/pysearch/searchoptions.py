# -*- coding: utf-8 -*-
"""
###############################################################################
#
# searchoptions.py
#
# class SearchOptions: defines the available command-line options and
#                      corresponding utility methods
#
###############################################################################
"""
import importlib.resources
import json
import os
import sys
from collections import deque
from io import StringIO
from typing import List

from pyfind import common
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
                settings.set_property('first_match', not b),
            'archivesonly':
                lambda b, settings:
                settings.set_property('archives_only', b),
            'colorize':
                lambda b, settings:
                settings.set_property('colorize', b),
            'debug':
                lambda b, settings:
                settings.set_property('debug', b),
            'excludehidden':
                lambda b, settings:
                settings.set_property('include_hidden', not b),
            'firstmatch':
                lambda b, settings:
                settings.set_property('first_match', b),
            'help':
                lambda b, settings:
                settings.set_property('print_usage', b),
            'includehidden':
                lambda b, settings:
                settings.set_property('include_hidden', b),
            'multilinesearch':
                lambda b, settings:
                settings.set_property('multi_line_search', b),
            'nocolorize':
                lambda b, settings:
                settings.set_property('colorize', not b),
            'noprintdirs':
                lambda b, settings:
                settings.set_property('print_dirs', not b),
            'noprintfiles':
                lambda b, settings:
                settings.set_property('print_files', not b),
            'noprintlines':
                lambda b, settings:
                settings.set_property('print_lines', not b),
            'noprintmatches':
                lambda b, settings:
                settings.set_property('print_results', not b),
            'norecursive':
                lambda b, settings:
                settings.set_property('recursive', not b),
            'nosearcharchives':
                lambda b, settings:
                settings.set_property('search_archives', not b),
            'printdirs':
                lambda b, settings:
                settings.set_property('print_dirs', b),
            'printfiles':
                lambda b, settings:
                settings.set_property('print_files', b),
            'printlines':
                lambda b, settings:
                settings.set_property('print_lines', b),
            'printmatches':
                lambda b, settings:
                settings.set_property('print_results', b),
            'recursive':
                lambda b, settings:
                settings.set_property('recursive', b),
            'searcharchives':
                lambda b, settings:
                settings.set_property('search_archives', b),
            'sort-ascending':
                lambda b, settings:
                settings.set_property('sort_descending', not b),
            'sort-caseinsensitive':
                lambda b, settings:
                settings.set_property('sort_case_insensitive', b),
            'sort-casesensitive':
                lambda b, settings:
                settings.set_property('sort_case_insensitive', not b),
            'sort-descending':
                lambda b, settings:
                settings.set_property('sort_descending', b),
            'uniquelines':
                lambda b, settings:
                settings.set_property('unique_lines', b),
            'verbose':
                lambda b, settings:
                settings.set_property('verbose', b),
            'version':
                lambda b, settings:
                settings.set_property('print_version', b)
        }

        self.__coll_arg_dict = {
            'in-archiveext':
                lambda x, settings:
                settings.add_strs_to_set(x, 'in_archive_extensions'),
            'in-archivefilepattern':
                lambda x, settings:
                settings.add_patterns(x, 'in_archive_file_patterns'),
            'in-dirpattern':
                lambda x, settings:
                settings.add_patterns(x, 'in_dir_patterns'),
            'in-ext':
                lambda x, settings:
                settings.add_strs_to_set(x, 'in_extensions'),
            'in-filepattern':
                lambda x, settings:
                settings.add_patterns(x, 'in_file_patterns'),
            'in-filetype':
                lambda x, settings:
                settings.add_file_types(x, 'in_file_types'),
            'in-linesafterpattern':
                lambda x, settings:
                settings.add_patterns(x, 'in_lines_after_patterns'),
            'in-linesbeforepattern':
                lambda x, settings:
                settings.add_patterns(x, 'in_lines_before_patterns'),
            'linesaftertopattern':
                lambda x, settings:
                settings.add_patterns(x, 'lines_after_to_patterns'),
            'linesafteruntilpattern':
                lambda x, settings:
                settings.add_patterns(x, 'lines_after_until_patterns'),
            'out-archiveext':
                lambda x, settings:
                settings.add_strs_to_set(x, 'out_archive_extensions'),
            'out-archivefilepattern':
                lambda x, settings:
                settings.add_patterns(x, 'out_archive_file_patterns'),
            'out-dirpattern':
                lambda x, settings:
                settings.add_patterns(x, 'out_dir_patterns'),
            'out-ext':
                lambda x, settings:
                settings.add_strs_to_set(x, 'out_extensions'),
            'out-filepattern':
                lambda x, settings:
                settings.add_patterns(x, 'out_file_patterns'),
            'out-filetype':
                lambda x, settings:
                settings.add_file_types(x, 'out_file_types'),
            'out-linesafterpattern':
                lambda x, settings:
                settings.add_patterns(x, 'out_lines_after_patterns'),
            'out-linesbeforepattern':
                lambda x, settings:
                settings.add_patterns(x, 'out_lines_before_patterns'),
            'path':
                lambda x, settings:
                settings.add_path(x),
            'searchpattern':
                lambda x, settings:
                settings.add_patterns(x, 'search_patterns'),
            'sort-by':
                lambda x, settings:
                settings.set_sort_by(x),
        }

        self.__dt_arg_dict = {
            'lastmod-after':
                lambda x, settings:
                settings.set_property('lastmod_after', x),
            'lastmod-before':
                lambda x, settings:
                settings.set_property('lastmod_before', x),
            'maxlastmod':
                lambda x, settings:
                settings.set_property('max_last_mod', x),
            'minlastmod':
                lambda x, settings:
                settings.set_property('min_last_mod', x),
        }

        self.__int_arg_dict = {
            'linesafter':
                lambda x, settings:
                settings.set_property('lines_after', int(x)),
            'linesbefore':
                lambda x, settings:
                settings.set_property('lines_before', int(x)),
            'maxdepth':
                lambda x, settings:
                settings.set_property('max_depth', int(x)),
            'maxlinelength':
                lambda x, settings:
                settings.set_property('max_line_length', int(x)),
            'maxsize':
                lambda x, settings:
                settings.set_property('max_size', int(x)),
            'mindepth':
                lambda x, settings:
                settings.set_property('min_depth', int(x)),
            'minsize':
                lambda x, settings:
                settings.set_property('min_size', int(x)),
        }

        self.__str_arg_dict = {
            'encoding':
                lambda x, settings:
                settings.set_property('text_file_encoding', x),
            'path':
                lambda x, settings:
                settings.paths.add(x),
        }

        self.__long_arg_dict = {}

    def settings_from_file(self, file_path: str, settings: SearchSettings):
        assert os.path.exists(file_path), \
            'Settings file not found: {0:s}'.format(file_path)
        with open(file_path) as f:
            json_str = f.read()
        self.settings_from_json(json_str, settings)

    def settings_from_json(self, json_str: str, settings: SearchSettings):
        json_dict = json.loads(json_str)
        for arg in json_dict:
            if arg in self.__bool_arg_dict:
                self.__bool_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__coll_arg_dict:
                self.__coll_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__dt_arg_dict:
                self.__dt_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__int_arg_dict:
                self.__int_arg_dict[arg](json_dict[arg], settings)
            elif arg in self.__str_arg_dict:
                self.__str_arg_dict[arg](json_dict[arg], settings)
            else:
                raise SearchException(f'Invalid option: {arg}')

    def __set_options_from_json(self):
        data = importlib.resources.files('pysearch').joinpath('data')
        search_options_json = data.joinpath('searchoptions.json').read_text()
        search_options_dict = json.loads(search_options_json)
        for search_option_obj in search_options_dict['searchoptions']:
            long_arg = search_option_obj['long']
            short_arg = ''
            if 'short' in search_option_obj:
                short_arg = search_option_obj['short']
            desc = search_option_obj['desc']
            if long_arg in self.__bool_arg_dict:
                func = self.__bool_arg_dict[long_arg]
            elif long_arg in self.__coll_arg_dict:
                func = self.__coll_arg_dict[long_arg]
            elif long_arg in self.__dt_arg_dict:
                func = self.__dt_arg_dict[long_arg]
            elif long_arg in self.__int_arg_dict:
                func = self.__int_arg_dict[long_arg]
            elif long_arg in self.__str_arg_dict:
                func = self.__str_arg_dict[long_arg]
            elif long_arg in self.__str_arg_dict:
                func = self.__str_arg_dict[long_arg]
            elif long_arg == 'settings-file':
                func = self.settings_from_file
            else:
                raise SearchException(f'Unknown search option: {long_arg}')
            self.options.append(SearchOption(short_arg, long_arg, desc, func))
            self.__long_arg_dict[long_arg] = long_arg
            if short_arg:
                self.__long_arg_dict[short_arg] = long_arg

    def search_settings_from_args(self, args: List[str]) -> SearchSettings:
        """Returns a SearchSettings instance for a given list of args"""
        # default print_results to True since running from command line
        settings = SearchSettings(print_results=True)
        return self.update_settings_from_args(settings, args)

    def update_settings_from_args(self, settings: SearchSettings, args: List[str]) -> SearchSettings:
        """Updates a SearchSettings instance from a given list of args"""
        arg_deque = deque(args)
        while arg_deque:
            arg = arg_deque.popleft()
            if arg.startswith('-'):
                while arg and arg.startswith('-'):
                    arg = arg[1:]
                if arg in self.__long_arg_dict:
                    long_arg = self.__long_arg_dict[arg]
                    if long_arg in self.__bool_arg_dict:
                        self.__bool_arg_dict[long_arg](True, settings)
                        if long_arg in ('help', 'version'):
                            return settings
                    elif long_arg in self.__coll_arg_dict or \
                            long_arg in self.__dt_arg_dict or \
                            long_arg in self.__int_arg_dict or \
                            long_arg in self.__str_arg_dict or \
                            long_arg == 'settings-file':
                        if arg_deque:
                            arg_val = arg_deque.popleft()
                            if long_arg in self.__coll_arg_dict:
                                self.__coll_arg_dict[long_arg](arg_val, settings)
                            elif long_arg in self.__dt_arg_dict:
                                self.__dt_arg_dict[long_arg](
                                    common.parse_datetime_str(arg_val), settings)
                            elif long_arg in self.__int_arg_dict:
                                invalid_int = False
                                try:
                                    i = int(arg_val)
                                except ValueError:
                                    invalid_int = True
                                else:
                                    if i < 0:
                                        invalid_int = True
                                if invalid_int:
                                    err = f'Invalid value for option {arg}: {arg_val}'
                                    raise SearchException(err)
                                self.__int_arg_dict[long_arg](arg_val, settings)
                            elif long_arg in self.__str_arg_dict:
                                self.__str_arg_dict[long_arg](arg_val, settings)
                            elif long_arg == 'settings-file':
                                self.settings_from_file(arg_val, settings)
                        else:
                            raise SearchException(f'Missing value for option {arg}')
                    else:
                        raise SearchException(f'Invalid option: {arg}')
                else:
                    raise SearchException(f'Invalid option: {arg}')
            else:
                settings.add_path(arg)
        return settings

    def usage(self, exit_code: int = 0):
        """Print the usage string and exit"""
        print(self.__get_usage_string())
        sys.exit(exit_code)

    def __get_usage_string(self):
        sio = StringIO()
        sio.write('Usage:\n')
        sio.write(
            ' pysearch [options] -s <searchpattern> <path> [<path> ...]\n\nOptions:\n')
        opt_pairs = []
        longest = 0
        for opt in sorted(self.options, key=lambda o: o.sort_arg):
            opt_string = ''
            if opt.short_arg:
                opt_string += f'-{opt.short_arg},'
            opt_string += f'--{opt.long_arg}'
            if len(opt_string) > longest:
                longest = len(opt_string)
            opt_pairs.append((opt_string, opt.desc))
        format_string = ' {0:<' + str(longest) + 's}  {1:s}\n'
        for opt_pair in opt_pairs:
            sio.write(format_string.format(opt_pair[0], opt_pair[1]))
        usage = sio.getvalue()
        sio.close()
        return usage
