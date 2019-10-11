# -*- coding: utf-8 -*-
###############################################################################
#
# searchsettings.py
#
# class SearchSettings: encapsulates search settings
#
###############################################################################
import re
from typing import Pattern, Set

from .filetypes import FileType
from .searchexception import SearchException


class SearchSettings(object):
    """a class to encapsulate search settings for a particular search session"""

    def __init__(self, archivesonly: bool = False, debug: bool = False, excludehidden: bool = True,
                 firstmatch: bool = False, in_archiveextensions: Set[str] = None,
                 in_archivefilepatterns: Set[str] = None, in_dirpatterns: Set[Pattern] = None,
                 in_extensions: Set[str] = None, in_filepatterns: Set[str] = None, in_filetypes: Set[str] = None,
                 in_linesafterpatterns: Set[str] = None, in_linesbeforepatterns: Set[str] = None, linesafter: int = 0,
                 linesaftertopatterns: Set[str] = None, linesafteruntilpatterns: Set[str] = None, linesbefore: int = 0,
                 listdirs: bool = False, listfiles: bool = False, listlines: bool = False, maxlinelength: int = 150,
                 multilinesearch: bool = False, out_archivefilepatterns: Set[str] = None,
                 out_archiveextensions: Set[str] = None, out_dirpatterns: Set[Pattern] = None,
                 out_extensions: Set[str] = None, out_filepatterns: Set[str] = None, out_filetypes: Set[str] = None,
                 out_linesafterpatterns: Set[str] = None, out_linesbeforepatterns: Set[str] = None,
                 printresults: bool = True, printusage: bool = False, printversion: bool = False,
                 recursive: bool = True, searcharchives: bool = False, searchpatterns: Set[str] = None,
                 startpath: str = '', uniquelines: bool = True, verbose: bool = True):
        self.archivesonly = archivesonly
        self.debug = debug
        self.excludehidden = excludehidden
        self.firstmatch = firstmatch
        self.in_archiveextensions = in_archiveextensions if in_archiveextensions else set([])
        self.in_archivefilepatterns = in_archivefilepatterns if in_archivefilepatterns else set([])
        self.in_dirpatterns = in_dirpatterns if in_dirpatterns else set([])
        self.in_extensions = in_extensions if in_extensions else set([])
        self.in_filepatterns = in_filepatterns if in_filepatterns else set([])
        self.in_filetypes = in_filetypes if in_filetypes else set([])
        self.in_linesafterpatterns = in_linesafterpatterns if in_linesafterpatterns else set([])
        self.in_linesbeforepatterns = in_linesbeforepatterns if in_linesbeforepatterns else set([])
        self.linesafter = linesafter
        self.linesaftertopatterns = linesaftertopatterns if linesaftertopatterns else set([])
        self.linesafteruntilpatterns = linesafteruntilpatterns if linesafteruntilpatterns else set([])
        self.linesbefore = linesbefore
        self.listdirs = listdirs
        self.listfiles = listfiles
        self.listlines = listlines
        self.maxlinelength = maxlinelength
        self.multilinesearch = multilinesearch
        self.out_archiveextensions = out_archiveextensions if out_archiveextensions else set([])
        self.out_archivefilepatterns = out_archivefilepatterns if out_archivefilepatterns else set([])
        self.out_dirpatterns = out_dirpatterns if out_dirpatterns else set([])
        self.out_extensions = out_extensions if out_extensions else set([])
        self.out_filepatterns = out_filepatterns if out_filepatterns else set([])
        self.out_filetypes = out_filetypes if out_filetypes else set([])
        self.out_linesafterpatterns = out_linesafterpatterns if out_linesafterpatterns else set([])
        self.out_linesbeforepatterns = out_linesbeforepatterns if out_linesbeforepatterns else set([])
        self.recursive = recursive
        self.printresults = printresults
        self.printusage = printusage
        self.printversion = printversion
        self.searchpatterns = searchpatterns if searchpatterns else set([])
        self.searcharchives = searcharchives
        self.startpath = startpath
        self.uniquelines = uniquelines
        self.verbose = verbose

    def add_exts(self, exts, ext_set_name: str):
        if type(exts) is list:
            self.__dict__[ext_set_name] = self.__dict__[ext_set_name].union(exts)
        elif type(exts) is str:
            ext_set = set([ext for ext in exts.split(',') if ext])
            self.__dict__[ext_set_name] = self.__dict__[ext_set_name].union(ext_set)

    def add_patterns(self, patterns, pattern_set_name: str):
        compile_flag = re.S | re.U
        if type(patterns) is list:
            pattern_set = set([re.compile(p, compile_flag) for p in patterns])
            self.__dict__[pattern_set_name] = self.__dict__[pattern_set_name].union(pattern_set)
        elif type(patterns) is str:
            self.__dict__[pattern_set_name].add(re.compile(patterns, compile_flag))
        else:
            raise SearchException('patterns is an unknown type')

    def add_filetypes(self, filetypes, filetype_set_name: str):
        # filetype_set = set()
        if type(filetypes) is list:
            filetype_set = set([FileType.from_name(ft) for ft in filetypes])
        elif type(filetypes) is str:
            filetype_set = set([FileType.from_name(ft) for ft in filetypes.split(',') if ft])
        else:
            raise SearchException('filetypes is an unknown type')
        self.__dict__[filetype_set_name] = self.__dict__[filetype_set_name].union(filetype_set)

    def set_property(self, name: str, val):
        self.__dict__[name] = val
        # some trues trigger others
        if type(val) is bool and val:
            if name == 'archivesonly':
                self.searcharchives = True
            elif name == 'debug':
                self.verbose = True

    def set_properties(self, propdict):
        for p in propdict:
            self.set_property(p, propdict[p])

    def __str__(self):
        all_props = {p for p in self.__dict__.keys() if not callable(self.__dict__[p])}
        print_dict = {}
        s = '{0}('.format(self.__class__.__name__)
        for p in sorted(all_props):
            val = self.__dict__[p]
            if type(val) == set:
                if len(val) > 0 and hasattr(list(val)[0], 'pattern'):
                    print_dict[p] = str([x.pattern for x in val])
                else:
                    print_dict[p] = str(list(val))
            elif type(val) is str:
                if val:
                    print_dict[p] = '"{0}"'.format(val)
                else:
                    print_dict[p] = '""'
            else:
                print_dict[p] = '{0!s}'.format(val)
        next_elem = 0
        for p in sorted(print_dict.keys()):
            if next_elem:
                s += ', '
            s += '{0}: {1}'.format(p, print_dict[p])
            next_elem += 1
        s += ')'
        return s
