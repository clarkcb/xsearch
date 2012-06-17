################################################################################
#
# searchresult.py
#
# class SearchResult: encapsulates a search result
#
################################################################################

class SearchResult:
    '''encapsulates a search result'''
    DEBUG = False

    def __init__(self, **kargs):
        self.pattern = ''
        self.filename = ''
        self.linenum = 0
        self.line = ''
        self.contained = ''
        self.lines_before =[]
        self.lines_after =[]
        self.__dict__.update(kargs)
        if self.DEBUG:
            print self.__str__()

    def __str__(self):
        s = 'SearchResult('
        s += 'pattern: "%s"' % self.pattern
        s += ', filename: "%s"' % self.filename
        s += ', linenum: %d' % self.linenum
        s += ', line: "%s"' % self.line.strip()
        if self.contained:
            s += ', contained: "%s"' % self.contained
        if self.lines_before:
            s += ', lines_before: %s' % str(self.lines_before)
        if self.lines_after:
            s += ', lines_after: %s' % str(self.lines_after)
        return s

    def __repr__(self):
        s = '<%s' % self.__class__.__name__
        s += ' pattern: "%s"' % self.pattern
        s += ', filename: "%s"' % self.filename
        s += ', linenum: %d' % self.linenum
        s += ', line: "%s"' % self.line.replace("\n", "\\n")
        if self.contained:
            s += ', contained: "%s"' % self.contained
        if self.lines_before:
            s += ', lines_before: %s' % repr(self.lines_before)
        if self.lines_after:
            s += ', lines_after: %s' % repr(self.lines_after)
        s += '>'
        return s

