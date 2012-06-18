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
        self._searchpatterns = set()
        self.ci_searchpatterns = set()
        self.cs_searchpatterns = set()
        self.firstmatch = False
        self.in_dirpatterns = set()
        self.in_extensions = set()
        self.in_filepatterns = set()
        self.linesafterfilters = []
        self.linesaftersearches = []
        self.linesbeforefilters = []
        self.linesbeforesearches = []
        self.listfiles = False
        self.listlines = False
        self.multilinesearch = False
        self.numlinesafter = 0
        self.numlinesbefore = 0
        self.out_dirpatterns = set()
        self.out_extensions = set()
        self.out_filepatterns = set()
        self.printresults = True
        self.re_search_set = set()
        self.searchcompressed = True
        self.startpath = None
        self.debug = False
        self.dotiming = False
        self.printusage = False
        self.printversion = False
        self.verbose = False

    def add_searchpattern(self, pattern, casesensitive=True):
        if casesensitive:
            self.cs_searchpatterns.add(pattern)
            self._searchpatterns.add(pattern)
            self.re_search_set.add(re.compile(pattern, re.S))
        else:
            self.ci_searchpatterns.add(pattern)
            self._searchpatterns.add(pattern)
            self.re_search_set.add(re.compile(pattern, re.I | re.S))

    def set_property(self, name, val):
        self.__dict__[name] = val

    @property
    def searchpatterns(self):
        return self._searchpatterns

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
        if self.cs_searchpatterns:
            s += ", cs_searchpatterns: %s" % str(self.cs_searchpatterns)
        if self.ci_searchpatterns:
            s += ", ci_searchpatterns: %s" % str(self.ci_searchpatterns)
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

