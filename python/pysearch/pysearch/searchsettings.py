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

from pyfind import FindSettings, FindException, FileType

# from .filetypes import FileType
from .searchexception import SearchException

PatternSet = Set[Pattern]


class SearchSettings(FindSettings):
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
        FindSettings.__init__(self, archivesonly=archives_only, colorize=colorize, debug=debug,
                              excludehidden=exclude_hidden, in_archiveextensions=in_archive_extensions,
                              in_archivefilepatterns=in_archive_file_patterns, in_dirpatterns=in_dir_patterns,
                              in_extensions=in_extensions, in_filepatterns=in_file_patterns, in_filetypes=in_file_types,
                              listdirs=list_dirs, listfiles=list_files, out_archiveextensions=out_archive_extensions,
                              out_archivefilepatterns=out_archive_file_patterns, out_dirpatterns=out_dir_patterns,
                              out_extensions=out_extensions, out_filepatterns=out_file_patterns,
                              out_filetypes=out_file_types, paths=paths, printusage=print_usage,
                              printversion=print_version, recursive=recursive, verbose=verbose)
        self.firstmatch = first_match
        self.in_linesafterpatterns: PatternSet = in_lines_after_patterns if in_lines_after_patterns else set()
        self.in_linesbeforepatterns: PatternSet = in_lines_before_patterns if in_lines_before_patterns else set()
        self.linesafter = lines_after
        self.linesaftertopatterns: PatternSet = lines_after_to_patterns if lines_after_to_patterns else set()
        self.linesafteruntilpatterns: PatternSet = lines_after_until_patterns if lines_after_until_patterns else set()
        self.linesbefore = lines_before
        self.listlines = list_lines
        self.maxlinelength = max_line_length
        self.multilinesearch = multi_line_search
        self.out_linesafterpatterns: PatternSet = out_lines_after_patterns if out_lines_after_patterns else set()
        self.out_linesbeforepatterns: PatternSet = out_lines_before_patterns if out_lines_before_patterns else set()
        self.paths = paths if paths else set()
        self.recursive = recursive
        self.printresults = print_results
        self.printusage = print_usage
        self.printversion = print_version
        self.searchpatterns = search_patterns if search_patterns else set()
        self.searcharchives = search_archives
        self.textfileencoding = text_file_encoding
        self.uniquelines = unique_lines

    def add_exts(self, exts, ext_set_name: str):
        try:
            FindSettings.add_exts(self, exts, ext_set_name)
        except FindException as e:
            raise SearchException(str(e))

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
