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
        self.__bool_action_dict = {
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
            'followsymlinks':
                lambda b, settings:
                settings.set_property('follow_symlinks', b),
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
            'nofollowsymlinks':
                lambda b, settings:
                settings.set_property('follow_symlinks', not b),
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

        self.__str_action_dict = {
            'encoding':
                lambda s, settings:
                settings.set_property('text_file_encoding', s),
            'in-archiveext':
                lambda s, settings:
                settings.add_strs_to_set(s, 'in_archive_extensions'),
            'in-archivefilepattern':
                lambda s, settings:
                settings.add_patterns(s, 'in_archive_file_patterns'),
            'in-dirpattern':
                lambda s, settings:
                settings.add_patterns(s, 'in_dir_patterns'),
            'in-ext':
                lambda s, settings:
                settings.add_strs_to_set(s, 'in_extensions'),
            'in-filepattern':
                lambda s, settings:
                settings.add_patterns(s, 'in_file_patterns'),
            'in-filetype':
                lambda s, settings:
                settings.add_file_types(s, 'in_file_types'),
            'in-linesafterpattern':
                lambda s, settings:
                settings.add_patterns(s, 'in_lines_after_patterns'),
            'in-linesbeforepattern':
                lambda s, settings:
                settings.add_patterns(s, 'in_lines_before_patterns'),
            'linesaftertopattern':
                lambda s, settings:
                settings.add_patterns(s, 'lines_after_to_patterns'),
            'linesafteruntilpattern':
                lambda s, settings:
                settings.add_patterns(s, 'lines_after_until_patterns'),
            'out-archiveext':
                lambda s, settings:
                settings.add_strs_to_set(s, 'out_archive_extensions'),
            'out-archivefilepattern':
                lambda s, settings:
                settings.add_patterns(s, 'out_archive_file_patterns'),
            'out-dirpattern':
                lambda s, settings:
                settings.add_patterns(s, 'out_dir_patterns'),
            'out-ext':
                lambda s, settings:
                settings.add_strs_to_set(s, 'out_extensions'),
            'out-filepattern':
                lambda s, settings:
                settings.add_patterns(s, 'out_file_patterns'),
            'out-filetype':
                lambda s, settings:
                settings.add_file_types(s, 'out_file_types'),
            'out-linesafterpattern':
                lambda s, settings:
                settings.add_patterns(s, 'out_lines_after_patterns'),
            'out-linesbeforepattern':
                lambda s, settings:
                settings.add_patterns(s, 'out_lines_before_patterns'),
            'path':
                lambda s, settings:
                settings.add_path(s),
            'searchpattern':
                lambda s, settings:
                settings.add_patterns(s, 'search_patterns'),
            'sort-by':
                lambda s, settings:
                settings.set_sort_by(s),
        }

        self.__dt_action_dict = {
            'lastmod-after':
                lambda dt, settings:
                settings.set_property('lastmod_after', dt),
            'lastmod-before':
                lambda dt, settings:
                settings.set_property('lastmod_before', dt),
            'maxlastmod':
                lambda dt, settings:
                settings.set_property('max_last_mod', dt),
            'minlastmod':
                lambda dt, settings:
                settings.set_property('min_last_mod', dt),
        }

        self.__int_action_dict = {
            'linesafter':
                lambda i, settings:
                settings.set_property('lines_after', i),
            'linesbefore':
                lambda i, settings:
                settings.set_property('lines_before', i),
            'maxdepth':
                lambda i, settings:
                settings.set_property('max_depth', i),
            'maxlinelength':
                lambda i, settings:
                settings.set_property('max_line_length', i),
            'maxsize':
                lambda i, settings:
                settings.set_property('max_size', i),
            'mindepth':
                lambda i, settings:
                settings.set_property('min_depth', i),
            'minsize':
                lambda i, settings:
                settings.set_property('min_size', i),
        }

        self.__long_arg_dict = {}

    def settings_from_file(self, file_path: str, settings: SearchSettings):
        """Read settings from a JSON file"""
        assert os.path.exists(file_path), f'Settings file not found: {file_path}'
        with open(file_path, encoding='UTF-8') as f:
            json_str = f.read()
        self.settings_from_json(json_str, settings)

    def settings_from_json(self, json_str: str, settings: SearchSettings):
        """Read settings from a JSON string"""
        json_dict = json.loads(json_str)
        for arg in json_dict:
            if arg in self.__bool_action_dict:
                self.__bool_action_dict[arg](json_dict[arg], settings)
            elif arg in self.__str_action_dict:
                self.__str_action_dict[arg](json_dict[arg], settings)
            elif arg in self.__dt_action_dict:
                self.__dt_action_dict[arg](json_dict[arg], settings)
            elif arg in self.__int_action_dict:
                self.__int_action_dict[arg](json_dict[arg], settings)
            elif arg in self.__str_action_dict:
                self.__str_action_dict[arg](json_dict[arg], settings)
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
            if long_arg in self.__bool_action_dict:
                func = self.__bool_action_dict[long_arg]
            elif long_arg in self.__str_action_dict:
                func = self.__str_action_dict[long_arg]
            elif long_arg in self.__dt_action_dict:
                func = self.__dt_action_dict[long_arg]
            elif long_arg in self.__int_action_dict:
                func = self.__int_action_dict[long_arg]
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
                    if long_arg in self.__bool_action_dict:
                        self.__bool_action_dict[long_arg](True, settings)
                        if long_arg in ('help', 'version'):
                            return settings
                    elif long_arg in self.__str_action_dict or \
                            long_arg in self.__dt_action_dict or \
                            long_arg in self.__int_action_dict or \
                            long_arg == 'settings-file':
                        if arg_deque:
                            arg_val = arg_deque.popleft()
                            if long_arg in self.__str_action_dict:
                                self.__str_action_dict[long_arg](arg_val, settings)
                            elif long_arg in self.__dt_action_dict:
                                self.__dt_action_dict[long_arg](
                                    common.parse_datetime_str(arg_val), settings)
                            elif long_arg in self.__int_action_dict:
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
                                self.__int_action_dict[long_arg](arg_val, settings)
                            elif long_arg in self.__str_action_dict:
                                self.__str_action_dict[long_arg](arg_val, settings)
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
