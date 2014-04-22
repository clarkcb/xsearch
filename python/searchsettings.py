################################################################################
#
# searchsettings.py
#
# class SearchSettings: encapsulates search settings
#
################################################################################
import re

class SearchSettings:
    """a class to encapsulate search settings for a particular search session"""

    _extension_set_names = 'in_extensions out_extensions'.split()
    _pattern_set_names = '''in_dirpatterns out_dirpatterns
                            in_filepatterns out_filepatterns
                            in_linesafterpatterns out_linesafterpatterns
                            in_linesbeforepatterns out_linesbeforepatterns
                            searchpatterns'''.split()
    _props_with_defaults = {
        'casesensitive': True,
        'debug': False,
        'dotiming': False,
        'firstmatch': False,
        'includedefaults': True,
        'listdirs': False,
        'listfiles': False,
        'listlines': False,
        'multilinesearch': False,
        'numlinesafter': 0,
        'numlinesbefore': 0,
        'printresults': True,
        'printusage': False,
        'printversion': False,
        'searchcompressed': True,
        'verbose': False,
    }

    DEFAULT_OUT_DIRPATTERNS = (r'\bCVS$', r'\.git$', r'\.svn$')
    DEFAULT_OUT_FILEPATTERNS = (r'^\.DS_Store$',)

    def __init__(self):
        self.startpath = None
        for name in self._extension_set_names:
            self.__dict__[name] = set()
        for name in self._pattern_set_names:
            self.__dict__[name] = set()
        self.__dict__.update(self._props_with_defaults)
        if self.includedefaults:
            for out_dirpattern in self.DEFAULT_OUT_DIRPATTERNS:
                self.add_pattern(out_dirpattern, 'out_dirpatterns')
            for out_filepattern in self.DEFAULT_OUT_FILEPATTERNS:
                self.add_pattern(out_filepattern, 'out_filepatterns')

    def add_comma_delimited_exts(self, exts, ext_set_name):
        for x in [ext for ext in exts.split(',') if ext]:
            self.__dict__[ext_set_name].add(x)

    def add_pattern(self, pattern, pattern_set_name):
        assert pattern_set_name in self._pattern_set_names
        compile_flag = re.S | re.U
        if not self.casesensitive:
            compile_flag = re.I | compile_flag
        self.__dict__[pattern_set_name].add(re.compile(pattern, compile_flag))

    def set_property(self, name, val):
        self.__dict__[name] = val

    def __str__(self):
        s = '{0}(startpath: "{1}"'.format(self.__class__.__name__, self.startpath)
        for name in self._extension_set_names:
            if self.__dict__[name]:
                s += ', {0}: {1!s}'.format(name, self.__dict__[name])
        for name in self._pattern_set_names:
            if self.__dict__[name]:
                pattern_strings = [p.pattern for p in self.__dict__[name]]
                s += ', {0}: {1!s}'.format(name, pattern_strings)
        prop_names = self._props_with_defaults.keys()
        prop_names.sort()
        for name in prop_names:
            s += ', {0}: {1!s}'.format(name, self.__dict__[name])
        s += ')'
        return s
