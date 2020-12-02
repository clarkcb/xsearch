# -*- coding: utf-8 -*-
###############################################################################
#
# searchsettings.py
#
# class SearchSettings: encapsulates search settings
#
###############################################################################
import re
from typing import Any, Dict, Pattern, Set

from .filetypes import FileType
from .searchexception import SearchException

PatternSet = Set[Pattern]


class SearchSettings(object):
    """a class to encapsulate search settings for a particular search session"""

    __slots__ = [
        'archivesonly', 'colorize', 'debug', 'defaultsfiles', 'excludehidden', 'firstmatch',
        'in_archiveextensions', 'in_archivefilepatterns', 'in_dirpatterns', 'in_extensions',
        'in_filepatterns', 'in_filetypes', 'in_linesafterpatterns', 'in_linesbeforepatterns',
        'linesafter', 'linesaftertopatterns', 'linesafteruntilpatterns', 'linesbefore', 'listdirs',
        'listfiles', 'listlines', 'maxlinelength', 'multilinesearch', 'out_archivefilepatterns',
        'out_archiveextensions', 'out_dirpatterns', 'out_extensions', 'out_filepatterns',
        'out_filetypes', 'out_linesafterpatterns', 'out_linesbeforepatterns', 'printresults',
        'printusage', 'printversion', 'recursive', 'searcharchives', 'searchpatterns', 'startpath',
        'textfileencoding', 'uniquelines', 'verbose'
    ]

    def __init__(self, archivesonly: bool = False, colorize: bool = True, debug: bool = False,
                 defaultsfiles: bool = True, excludehidden: bool = True, firstmatch: bool = False,
                 in_archiveextensions: Set[str] = None, in_archivefilepatterns: PatternSet = None,
                 in_dirpatterns: PatternSet = None, in_extensions: Set[str] = None, in_filepatterns: PatternSet = None,
                 in_filetypes: Set[str] = None, in_linesafterpatterns: PatternSet = None,
                 in_linesbeforepatterns: PatternSet = None, linesafter: int = 0,
                 linesaftertopatterns: PatternSet = None, linesafteruntilpatterns: PatternSet = None,
                 linesbefore: int = 0, listdirs: bool = False, listfiles: bool = False, listlines: bool = False,
                 maxlinelength: int = 150, multilinesearch: bool = False, out_archivefilepatterns: PatternSet = None,
                 out_archiveextensions: Set[str] = None, out_dirpatterns: PatternSet = None,
                 out_extensions: Set[str] = None, out_filepatterns: PatternSet = None, out_filetypes: Set[str] = None,
                 out_linesafterpatterns: PatternSet = None, out_linesbeforepatterns: PatternSet = None,
                 printresults: bool = True, printusage: bool = False, printversion: bool = False,
                 recursive: bool = True, searcharchives: bool = False, searchpatterns: PatternSet = None,
                 startpath: str = '', textfileencoding: str = 'UTF-8', uniquelines: bool = False,
                 verbose: bool = False):
        self.archivesonly = archivesonly
        self.colorize = colorize
        self.defaultsfiles = defaultsfiles
        self.debug = debug
        self.excludehidden = excludehidden
        self.firstmatch = firstmatch
        self.in_archiveextensions = in_archiveextensions if in_archiveextensions else set([
        ])
        self.in_archivefilepatterns: PatternSet = in_archivefilepatterns if in_archivefilepatterns else set([
        ])
        self.in_dirpatterns: PatternSet = in_dirpatterns if in_dirpatterns else set([
        ])
        self.in_extensions = in_extensions if in_extensions else set([])
        self.in_filepatterns: PatternSet = in_filepatterns if in_filepatterns else set([
        ])
        self.in_filetypes = in_filetypes if in_filetypes else set([])
        self.in_linesafterpatterns: PatternSet = in_linesafterpatterns if in_linesafterpatterns else set([
        ])
        self.in_linesbeforepatterns: PatternSet = in_linesbeforepatterns if in_linesbeforepatterns else set([
        ])
        self.linesafter = linesafter
        self.linesaftertopatterns: PatternSet = linesaftertopatterns if linesaftertopatterns else set([
        ])
        self.linesafteruntilpatterns: PatternSet = linesafteruntilpatterns if linesafteruntilpatterns else set([
        ])
        self.linesbefore = linesbefore
        self.listdirs = listdirs
        self.listfiles = listfiles
        self.listlines = listlines
        self.maxlinelength = maxlinelength
        self.multilinesearch = multilinesearch
        self.out_archiveextensions = out_archiveextensions if out_archiveextensions else set([
        ])
        self.out_archivefilepatterns: PatternSet = out_archivefilepatterns if out_archivefilepatterns else set([
        ])
        self.out_dirpatterns: PatternSet = out_dirpatterns if out_dirpatterns else set([
        ])
        self.out_extensions = out_extensions if out_extensions else set([])
        self.out_filepatterns: PatternSet = out_filepatterns if out_filepatterns else set([
        ])
        self.out_filetypes = out_filetypes if out_filetypes else set([])
        self.out_linesafterpatterns: PatternSet = out_linesafterpatterns if out_linesafterpatterns else set([
        ])
        self.out_linesbeforepatterns: PatternSet = out_linesbeforepatterns if out_linesbeforepatterns else set([
        ])
        self.recursive = recursive
        self.printresults = printresults
        self.printusage = printusage
        self.printversion = printversion
        # self.searchpatterns: PatternSet = searchpatterns if searchpatterns else set([])
        self.searchpatterns = searchpatterns if searchpatterns else set([])
        self.searcharchives = searcharchives
        self.startpath = startpath
        self.textfileencoding = textfileencoding
        self.uniquelines = uniquelines
        self.verbose = verbose

    def add_exts(self, exts, ext_set_name: str):
        if isinstance(exts, list) or isinstance(exts, set):
            ext_set = getattr(self, ext_set_name)
            ext_set.update(exts)
        elif isinstance(exts, str):
            new_ext_set = set([ext for ext in exts.split(',') if ext])
            ext_set = getattr(self, ext_set_name)
            ext_set.update(new_ext_set)

    def add_patterns(self, patterns, pattern_set_name: str, compile_flag=re.S | re.U):
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

    def add_filetypes(self, filetypes, filetype_set_name: str):
        if isinstance(filetypes, list) or isinstance(filetypes, set):
            new_filetype_set = set([FileType.from_name(ft)
                                    for ft in filetypes])
        elif isinstance(filetypes, str):
            new_filetype_set = set([FileType.from_name(ft)
                                    for ft in filetypes.split(',') if ft])
        else:
            raise SearchException('filetypes is an unknown type')
        filetype_set = getattr(self, filetype_set_name)
        filetype_set.update(new_filetype_set)

    def set_property(self, name: str, val):
        setattr(self, name, val)
        # some trues trigger others
        if isinstance(val, bool) and val:
            if name == 'archivesonly':
                self.searcharchives = True
            elif name == 'debug':
                self.verbose = True

    def set_properties(self, propdict: Dict[str, Any]):
        for p in propdict.keys():
            self.set_property(p, propdict[p])

    def __str__(self):
        print_dict = {}
        s = '{0}('.format(self.__class__.__name__)
        for p in sorted(self.__slots__):
            val = getattr(self, p)
            if isinstance(val, set):
                if len(val) > 0 and hasattr(list(val)[0], 'pattern'):
                    print_dict[p] = str([x.pattern for x in val])
                else:
                    print_dict[p] = str(list(val))
            elif isinstance(val, str):
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
