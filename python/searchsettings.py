################################################################################
#
# searchsettings.py
#
# class SearchSettings: encapsulates search settings
#
################################################################################
import re

class SearchSettings:
    '''a class to encapsulate search settings for a particular search session'''
    def __init__(self):
        self._pattern_set_names = '''in_dirpatterns out_dirpatterns
                                     in_filepatterns out_filepatterns
                                     in_linesafterpatterns out_linesafterpatterns
                                     in_linesbeforepatterns out_linesbeforepatterns
                                     searchpatterns'''.split()
        for name in self._pattern_set_names:
            self.__dict__[name] = set()
        self.in_extensions = set()
        self.out_extensions = set()

        self.casesensitive = True
        self.debug = False
        self.dotiming = False
        self.firstmatch = False
        self.listfiles = False
        self.listlines = False
        self.multilinesearch = False
        self.numlinesafter = 0
        self.numlinesbefore = 0
        self.printresults = True
        self.printusage = False
        self.printversion = False
        self.searchcompressed = True
        self.startpath = None
        self.verbose = False

    def add_pattern(self, pattern, pattern_set_name):
        if pattern_set_name in self._pattern_set_names:
            compile_flag = re.S | re.U
            if not self.casesensitive:
                compile_flag = re.I | compile_flag
            self.__dict__[pattern_set_name].add(re.compile(pattern, compile_flag))

    def set_property(self, name, val):
        self.__dict__[name] = val

    def __str__(self):
        s = 'SearchSettings(startpath: "%s"' % self.startpath
        if self.in_extensions:
            s += ", in_extensions: %s" % str(self.in_extensions)
        if self.out_extensions:
            s += ", out_extensions: %s" % str(self.out_extensions)
        if self.in_dirpatterns:
            s += ", in_dirpatterns: %s" % str(self.in_dirpatterns)
        if self.out_dirpatterns:
            s += ", out_dirpatterns: %s" % str(self.out_dirpatterns)
        if self.in_filepatterns:
            s += ", in_filepatterns: %s" % str(self.in_filepatterns)
        if self.out_filepatterns:
            s += ", out_filepatterns: %s" % str(self.out_filepatterns)
        if self.numlinesafter:
            s += ", numlinesafter: %d" % self.numlinesafter
        if self.numlinesbefore:
            s += ", numlinesbefore: %d" % self.numlinesbefore
        s += ", listfiles: %s" % self.listfiles
        s += ", listlines: %s" % self.listlines
        s += ", searchcompressed: %s" % self.searchcompressed
        s += ", dotiming: %s" % self.dotiming
        s += ", verbose: %s" % self.verbose
        s += ", debug: %s" % self.debug
        s += ")"
        return s

