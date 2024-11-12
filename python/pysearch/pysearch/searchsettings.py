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
from pathlib import Path
import re
from typing import Optional, Pattern

from pyfind import FindSettings, FindException, SortBy, list_to_str, FileType

from .searchexception import SearchException

PatternSet = set[Pattern]


class SearchSettings(FindSettings):
    """a class to encapsulate search settings for a particular search session"""

    __slots__ = FindSettings.__slots__ + [
        'colorize', 'first_match', 'in_lines_after_patterns', 'in_lines_before_patterns',
        'lines_after', 'lines_after_to_patterns', 'lines_after_until_patterns', 'lines_before',
        'max_line_length', 'multi_line_search', 'out_lines_after_patterns',
        'out_lines_before_patterns', 'print_lines', 'print_results', 'search_archives',
        'search_patterns', 'text_file_encoding', 'unique_lines'
    ]

    def add_strs_to_set(self, strs: list[str] | set[str] | str, set_name: str):
        try:
            FindSettings.add_strs_to_set(self, strs, set_name)
        except FindException as e:
            raise SearchException(str(e))

    def __init__(self,
                 archives_only: bool = False,
                 colorize: bool = True,
                 debug: bool = False,
                 first_match: bool = False,
                 follow_symlinks: bool = False,
                 in_archive_extensions: list[str] | set[str] | str = None,
                 in_archive_file_patterns: PatternSet = None,
                 include_hidden: bool = False,
                 in_dir_patterns: PatternSet = None,
                 in_extensions: list[str] | set[str] | str = None,
                 in_file_patterns: PatternSet = None,
                 in_file_types: list | set | str | FileType = None,
                 in_lines_after_patterns: PatternSet = None,
                 in_lines_before_patterns: PatternSet = None,
                 lines_after: int = 0,
                 lines_after_to_patterns: PatternSet = None,
                 lines_after_until_patterns: PatternSet = None,
                 lines_before: int = 0,
                 max_depth: int = -1,
                 max_last_mod: Optional[datetime] = None,
                 max_line_length: int = 150,
                 max_size: int = 0,
                 min_depth: int = -1,
                 min_last_mod: Optional[datetime] = None,
                 min_size: int = 0,
                 multi_line_search: bool = False,
                 out_archive_extensions: list[str] | set[str] | str = None,
                 out_archive_file_patterns: PatternSet = None,
                 out_dir_patterns: PatternSet = None,
                 out_extensions: list[str] | set[str] | str = None,
                 out_file_patterns: PatternSet = None,
                 out_file_types: list | set | str | FileType = None,
                 out_lines_after_patterns: PatternSet = None,
                 out_lines_before_patterns: PatternSet = None,
                 paths: list[Path] | set[Path] | list[str] | set[str] | str = None,
                 print_dirs: bool = False,
                 print_files: bool = False,
                 print_lines: bool = False,
                 print_results: bool = True,
                 print_usage: bool = False,
                 print_version: bool = False,
                 recursive: bool = True,
                 search_archives: bool = False,
                 search_patterns: PatternSet = None,
                 sort_by: SortBy = SortBy.FILEPATH,
                 sort_case_insensitive: bool = False,
                 sort_descending: bool = False,
                 text_file_encoding: str = 'UTF-8',
                 unique_lines: bool = False,
                 verbose: bool = False):
        FindSettings.__init__(self,
                              archives_only=archives_only,
                              debug=debug,
                              # find_in_archives=search_archives,
                              follow_symlinks=follow_symlinks,
                              in_archive_extensions=in_archive_extensions,
                              in_archive_file_patterns=in_archive_file_patterns,
                              in_dir_patterns=in_dir_patterns,
                              in_extensions=in_extensions,
                              in_file_patterns=in_file_patterns,
                              in_file_types=in_file_types,
                              include_archives=search_archives,
                              include_hidden=include_hidden,
                              max_depth=max_depth,
                              max_last_mod=max_last_mod,
                              max_size=max_size,
                              min_depth=min_depth,
                              min_last_mod=min_last_mod,
                              min_size=min_size,
                              out_archive_extensions=out_archive_extensions,
                              out_archive_file_patterns=out_archive_file_patterns,
                              out_dir_patterns=out_dir_patterns,
                              out_extensions=out_extensions,
                              out_file_patterns=out_file_patterns,
                              out_file_types=out_file_types,
                              paths=paths,
                              print_dirs=print_dirs,
                              print_files=print_files,
                              print_usage=print_usage,
                              print_version=print_version,
                              recursive=recursive,
                              sort_by=sort_by,
                              sort_case_insensitive=sort_case_insensitive,
                              sort_descending=sort_descending,
                              verbose=verbose)
        self.colorize = colorize
        self.first_match = first_match
        self.in_lines_after_patterns: PatternSet = \
            in_lines_after_patterns if in_lines_after_patterns else set()
        self.in_lines_before_patterns: PatternSet = \
            in_lines_before_patterns if in_lines_before_patterns else set()
        self.lines_after = lines_after
        self.lines_after_to_patterns: PatternSet = \
            lines_after_to_patterns if lines_after_to_patterns else set()
        self.lines_after_until_patterns: PatternSet = \
            lines_after_until_patterns if lines_after_until_patterns else set()
        self.lines_before = lines_before
        self.max_line_length = max_line_length
        self.multi_line_search = multi_line_search
        self.out_lines_after_patterns: PatternSet = \
            out_lines_after_patterns if out_lines_after_patterns else set()
        self.out_lines_before_patterns: PatternSet = \
            out_lines_before_patterns if out_lines_before_patterns else set()
        self.print_lines = print_lines
        self.print_results = print_results
        self.search_patterns = search_patterns if search_patterns else set()
        self.search_archives = search_archives
        self.text_file_encoding = text_file_encoding
        self.unique_lines = unique_lines

    def add_patterns(self, patterns: list | set | str | Pattern, pattern_set_name: str, compile_flag=re.S | re.U):
        try:
            FindSettings.add_patterns(self, patterns, pattern_set_name, compile_flag)
        except FindException as e:
            raise SearchException(str(e))

    def set_property(self, name: str, val):
        FindSettings.set_property(self, name, val)
        # some trues trigger others
        if isinstance(val, bool) and val:
            if name == 'archives_only':
                self.search_archives = True
        # if self.search_archives:
        #     self.find_in_archives = True

    def __str__(self):
        return FindSettings.__str__(self)
