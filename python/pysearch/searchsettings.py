# -*- coding: utf-8 -*-
###############################################################################
#
# searchsettings.py
#
# class SearchSettings: encapsulates search settings
#
###############################################################################
import re
from filetypes import FileType
from searchexception import SearchException


class SearchSettings(object):
    """a class to encapsulate search settings for a particular search session"""

    _extension_set_names = set('''in_extensions out_extensions
                                  in_archiveextensions
                                  out_archiveextensions'''.split())
# TODO: move filetypes to own group
    _pattern_set_names = set('''in_dirpatterns out_dirpatterns
                                in_filepatterns out_filepatterns
                                in_filetypes out_filetypes
                                in_archivefilepatterns out_archivefilepatterns
                                in_linesafterpatterns out_linesafterpatterns
                                in_linesbeforepatterns out_linesbeforepatterns
                                linesaftertopatterns linesafteruntilpatterns
                                searchpatterns'''.split())
    _props_with_defaults = {
        'archivesonly': False,
        'debug': False,
        'firstmatch': False,
        'excludehidden': True,
        'linesafter': 0,
        'linesbefore': 0,
        'listdirs': False,
        'listfiles': False,
        'listlines': False,
        'maxlinelength': 150,
        'multilinesearch': False,
        'printresults': True,
        'printusage': False,
        'printversion': False,
        'recursive': True,
        'searcharchives': False,
        'uniquelines': False,
        'verbose': False,
    }

    def __init__(self):
        self.startpath = ''
        for name in self._extension_set_names:
            self.__dict__[name] = set()
        for name in self._pattern_set_names:
            self.__dict__[name] = set()
        self.__dict__.update(self._props_with_defaults)

    def add_exts(self, exts, ext_set_name):
        if type(exts) is list:
            self.__dict__[ext_set_name] = self.__dict__[ext_set_name].union(exts)
        elif type(exts) is str:
            ext_set = set([ext for ext in exts.split(',') if ext])
            self.__dict__[ext_set_name] = self.__dict__[ext_set_name].union(ext_set)

    def add_patterns(self, patterns, pattern_set_name):
        assert pattern_set_name in self._pattern_set_names
        compile_flag = re.S | re.U
        if type(patterns) is list:
            pattern_set = set([re.compile(p, compile_flag) for p in patterns])
            self.__dict__[pattern_set_name] = self.__dict__[pattern_set_name].union(pattern_set)
        elif type(patterns) is str:
            self.__dict__[pattern_set_name].add(re.compile(patterns, compile_flag))
        else:
            raise SearchException('patterns is an unknown type')

    def add_filetypes(self, filetypes, filetype_set_name):
        # filetype_set = set()
        if type(filetypes) is list:
            filetype_set = set([FileType.from_name(ft) for ft in filetypes])
        elif type(filetypes) is str:
            filetype_set = set([FileType.from_name(ft) for ft in filetypes.split(',') if ft])
        else:
            raise SearchException('filetypes is an unknown type')
        self.__dict__[filetype_set_name] = self.__dict__[filetype_set_name].union(filetype_set)

    def set_property(self, name, val):
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
        all_props = {'startpath'} | self._extension_set_names | \
                    self._pattern_set_names | set(self._props_with_defaults.keys())
        print_dict = {}
        s = '{0}('.format(self.__class__.__name__)
        for p in sorted(all_props):
            val = self.__dict__[p]
            if type(val) == set:
                if p in self._pattern_set_names:
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
