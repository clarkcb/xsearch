# -*- coding: utf-8 -*-
"""
###############################################################################
#
# searchsettings.py
#
# class SearchSettings: encapsulates search settings
#
###############################################################################
"""
import re
from typing import Any, Dict, Pattern, Set

from .filetypes import FileType
from .searchexception import SearchException

PatternSet = Set[Pattern]


class SearchSettings(object):
    """a class to encapsulate search settings for a particular search session"""

    __slots__ = [
        'archives_only', 'colorize', 'debug', 'exclude_hidden', 'first_match',
        'in_archive_extensions', 'in_archive_file_patterns', 'in_dir_patterns', 'in_extensions',
        'in_file_patterns', 'in_file_types', 'in_lines_after_patterns', 'in_lines_before_patterns',
        'lines_after', 'lines_after_to_patterns', 'lines_after_until_patterns', 'lines_before',
        'list_dirs', 'list_files', 'list_lines', 'max_line_length', 'multi_line_search',
        'out_archive_file_patterns', 'out_archive_extensions', 'out_dir_patterns',
        'out_extensions', 'out_file_patterns', 'out_file_types', 'out_lines_after_patterns',
        'out_lines_before_patterns', 'paths', 'print_results', 'print_usage', 'print_version',
        'recursive', 'search_archives', 'search_patterns', 'text_file_encoding', 'unique_lines',
        'verbose'
    ]

    def __init__(self,
                 archives_only: bool = False,
                 colorize: bool = True,
                 debug: bool = False,
                 exclude_hidden: bool = True,
                 first_match: bool = False,
                 in_archive_extensions: Set[str] = None,
                 in_archive_file_patterns: PatternSet = None,
                 in_dir_patterns: PatternSet = None,
                 in_extensions: Set[str] = None,
                 in_file_patterns: PatternSet = None,
                 in_file_types: Set[str] = None,
                 in_lines_after_patterns: PatternSet = None,
                 in_lines_before_patterns: PatternSet = None,
                 lines_after: int = 0,
                 lines_after_to_patterns: PatternSet = None,
                 lines_after_until_patterns: PatternSet = None,
                 lines_before: int = 0,
                 list_dirs: bool = False,
                 list_files: bool = False,
                 list_lines: bool = False,
                 max_line_length: int = 150,
                 multi_line_search: bool = False,
                 out_archive_file_patterns: PatternSet = None,
                 out_archive_extensions: Set[str] = None,
                 out_dir_patterns: PatternSet = None,
                 out_extensions: Set[str] = None,
                 out_file_patterns: PatternSet = None,
                 out_file_types: Set[str] = None,
                 out_lines_after_patterns: PatternSet = None,
                 out_lines_before_patterns: PatternSet = None,
                 paths: Set[str] = None,
                 print_results: bool = True,
                 print_usage: bool = False,
                 print_version: bool = False,
                 recursive: bool = True,
                 search_archives: bool = False,
                 search_patterns: PatternSet = None,
                 text_file_encoding: str = 'UTF-8',
                 unique_lines: bool = False,
                 verbose: bool = False):
        self.archives_only = archives_only
        self.colorize = colorize
        self.debug = debug
        self.exclude_hidden = exclude_hidden
        self.first_match = first_match
        self.in_archive_extensions = in_archive_extensions if in_archive_extensions else set()
        self.in_archive_file_patterns: PatternSet = in_archive_file_patterns if in_archive_file_patterns else set()
        self.in_dir_patterns: PatternSet = in_dir_patterns if in_dir_patterns else set()
        self.in_extensions = in_extensions if in_extensions else set()
        self.in_file_patterns: PatternSet = in_file_patterns if in_file_patterns else set()
        self.in_file_types = in_file_types if in_file_types else set()
        self.in_lines_after_patterns: PatternSet = in_lines_after_patterns if in_lines_after_patterns else set()
        self.in_lines_before_patterns: PatternSet = in_lines_before_patterns if in_lines_before_patterns else set()
        self.lines_after = lines_after
        self.lines_after_to_patterns: PatternSet = lines_after_to_patterns if lines_after_to_patterns else set()
        self.lines_after_until_patterns: PatternSet = lines_after_until_patterns if lines_after_until_patterns else set()
        self.lines_before = lines_before
        self.list_dirs = list_dirs
        self.list_files = list_files
        self.list_lines = list_lines
        self.max_line_length = max_line_length
        self.multi_line_search = multi_line_search
        self.out_archive_extensions = out_archive_extensions if out_archive_extensions else set()
        self.out_archive_file_patterns: PatternSet = out_archive_file_patterns if out_archive_file_patterns else set()
        self.out_dir_patterns: PatternSet = out_dir_patterns if out_dir_patterns else set()
        self.out_extensions = out_extensions if out_extensions else set()
        self.out_file_patterns: PatternSet = out_file_patterns if out_file_patterns else set()
        self.out_file_types = out_file_types if out_file_types else set()
        self.out_lines_after_patterns: PatternSet = out_lines_after_patterns if out_lines_after_patterns else set()
        self.out_lines_before_patterns: PatternSet = out_lines_before_patterns if out_lines_before_patterns else set()
        self.paths = paths if paths else set()
        self.recursive = recursive
        self.print_results = print_results
        self.print_usage = print_usage
        self.print_version = print_version
        self.search_patterns = search_patterns if search_patterns else set()
        self.search_archives = search_archives
        self.text_file_encoding = text_file_encoding
        self.unique_lines = unique_lines
        self.verbose = verbose

    def add_exts(self, exts, ext_set_name: str):
        """Add one or more comma-separated extensions"""
        if isinstance(exts, list) or isinstance(exts, set):
            ext_set = getattr(self, ext_set_name)
            ext_set.update(exts)
        elif isinstance(exts, str):
            new_ext_set = set([ext for ext in exts.split(',') if ext])
            ext_set = getattr(self, ext_set_name)
            ext_set.update(new_ext_set)

    def add_patterns(self, patterns, pattern_set_name: str,
                     compile_flag=re.S | re.U):
        """Add patterns to patternset"""
        if isinstance(patterns, list) or isinstance(patterns, set):
            new_pattern_set = set([re.compile(p, compile_flag)
                                   for p in patterns])
            pattern_set = getattr(self, pattern_set_name)
            pattern_set.update(new_pattern_set)
        elif isinstance(patterns, str):
            pattern_set = getattr(self, pattern_set_name)
            pattern_set.add(re.compile(patterns, compile_flag))
        else:
            raise SearchException('patterns is an unknown type')

    def add_paths(self, paths):
        """Add one or more paths"""
        if isinstance(paths, (list, set)):
            self.paths.update(paths)
        elif isinstance(paths, str):
            self.paths.add(paths)
        else:
            raise SearchException('paths is an unknown type')

    def add_file_types(self, file_types, file_type_set_name: str):
        """Add one or more file_types"""
        if isinstance(file_types, list) or isinstance(file_types, set):
            new_file_type_set = set([FileType.from_name(ft)
                                    for ft in file_types])
        elif isinstance(file_types, str):
            new_file_type_set = set([FileType.from_name(ft)
                                    for ft in file_types.split(',') if ft])
        else:
            raise SearchException('file_types is an unknown type')
        file_type_set = getattr(self, file_type_set_name)
        file_type_set.update(new_file_type_set)

    def set_property(self, name: str, val):
        """Set a property"""
        setattr(self, name, val)
        # some trues trigger others
        if isinstance(val, bool) and val:
            if name == 'archives_only':
                self.search_archives = True
            elif name == 'debug':
                self.verbose = True

    def set_properties(self, propdict: Dict[str, Any]):
        """Set properties"""
        for p in propdict.keys():
            self.set_property(p, propdict[p])

    def __str__(self):
        print_dict = {}
        s = f'{self.__class__.__name__}('
        for p in sorted(self.__slots__):
            val = getattr(self, p)
            if isinstance(val, set):
                if len(val) > 0 and hasattr(list(val)[0], 'pattern'):
                    print_dict[p] = str([x.pattern for x in val])
                else:
                    print_dict[p] = str(list(val))
            elif isinstance(val, str):
                if val:
                    print_dict[p] = f'"{val}"'
                else:
                    print_dict[p] = '""'
            else:
                print_dict[p] = '{0!s}'.format(val)
        next_elem = 0
        for p in sorted(print_dict.keys()):
            if next_elem:
                s += ', '
            s += f'{p}: {print_dict[p]}'
            next_elem += 1
        s += ')'
        return s
