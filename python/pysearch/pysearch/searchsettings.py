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
from datetime import datetime
import re
from typing import Optional, Pattern, Set

from pyfind import FindSettings, FindException

from .searchexception import SearchException

PatternSet = Set[Pattern]


class SearchSettings(FindSettings):
    """a class to encapsulate search settings for a particular search session"""

    __slots__ = FindSettings.__slots__ + [
        'colorize', 'first_match', 'in_lines_after_patterns', 'in_lines_before_patterns',
        'lines_after', 'lines_after_to_patterns', 'lines_after_until_patterns', 'lines_before',
        'list_lines', 'max_line_length', 'multi_line_search', 'out_lines_after_patterns',
        'out_lines_before_patterns', 'search_archives', 'search_patterns', 'text_file_encoding',
        'unique_lines'
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
        FindSettings.__init__(self, archives_only=archives_only, debug=debug,
                              exclude_hidden=exclude_hidden, in_archiveextensions=in_archive_extensions,
                              in_archive_file_patterns=in_archive_file_patterns, in_dirpatterns=in_dir_patterns,
                              in_extensions=in_extensions, in_filepatterns=in_file_patterns, in_filetypes=in_file_types,
                              listdirs=list_dirs, listfiles=list_files, out_archiveextensions=out_archive_extensions,
                              out_archivefilepatterns=out_archive_file_patterns, out_dirpatterns=out_dir_patterns,
                              out_extensions=out_extensions, out_filepatterns=out_file_patterns,
                              out_filetypes=out_file_types, paths=paths, printusage=print_usage,
                              printversion=print_version, recursive=recursive, verbose=verbose)
        self.colorize = colorize
        self.first_match = first_match
        self.in_lines_after_patterns: PatternSet = in_lines_after_patterns if in_lines_after_patterns else set()
        self.in_lines_before_patterns: PatternSet = in_lines_before_patterns if in_lines_before_patterns else set()
        self.lines_after = lines_after
        self.lines_after_to_patterns: PatternSet = lines_after_to_patterns if lines_after_to_patterns else set()
        self.lines_after_until_patterns: PatternSet = lines_after_until_patterns if lines_after_until_patterns else set()
        self.lines_before = lines_before
        self.list_lines = list_lines
        self.max_line_length = max_line_length
        self.multi_line_search = multi_line_search
        self.out_lines_after_patterns: PatternSet = out_lines_after_patterns if out_lines_after_patterns else set()
        self.out_lines_before_patterns: PatternSet = out_lines_before_patterns if out_lines_before_patterns else set()
        self.search_patterns = search_patterns if search_patterns else set()
        self.search_archives = search_archives
        self.text_file_encoding = text_file_encoding
        self.unique_lines = unique_lines

    def add_exts(self, exts, ext_set_name: str):
        try:
            FindSettings.add_exts(self, exts, ext_set_name)
        except FindException as e:
            raise SearchException(str(e))

    def add_patterns(self, patterns, pattern_set_name: str, compile_flag=re.S | re.U):
        try:
            FindSettings.add_patterns(self, patterns, pattern_set_name, compile_flag)
        except FindException as e:
            raise SearchException(str(e))

    def add_filetypes(self, file_types, filetype_set_name: str):
        try:
            FindSettings.add_file_types(self, file_types, filetype_set_name)
        except FindException as e:
            raise SearchException(str(e))

    def set_property(self, name: str, val):
        FindSettings.set_property(self, name, val)
        # some trues trigger others
        if isinstance(val, bool) and val:
            if name == 'archives_only':
                self.search_archives = True

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
            elif isinstance(val, Optional[datetime]):
                if val:
                    print_dict[p] = f'"{val}"'
                else:
                    print_dict[p] = '0'
            else:
                print_dict[p] = f'{val}'
        next_elem = 0
        for p in sorted(print_dict.keys()):
            if next_elem:
                s += ', '
            s += f'{p}: {print_dict[p]}'
            next_elem += 1
        s += ')'
        return s
