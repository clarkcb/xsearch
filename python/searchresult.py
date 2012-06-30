################################################################################
#
# searchresult.py
#
# class SearchResult: encapsulates a search result
#
################################################################################
from cStringIO import StringIO

class SearchResult:
    '''encapsulates a search result'''

    def __init__(self, **kargs):
        self.pattern = ''
        self.filename = ''
        self.linenum = 0
        self.line = ''
        self.contained = ''
        self.lines_before =[]
        self.lines_after =[]
        self.__dict__.update(kargs)

    def __str__(self):
        sio = StringIO()
        if self.lines_before or self.lines_after:
            sio.write('%s\n' % ('=' * 80))
        sio.write('%s' % self.filename)
        if self.contained:
            sio.write(': %s' % self.contained)
        if self.lines_before or self.lines_after:
            sio.write('\n%s\n' % ('-' * 80))
            current_linenum = self.linenum
            if self.lines_before:
                current_linenum -= len(self.lines_before)
                for line_before in self.lines_before:
                    sio.write('  %d | %s' % (current_linenum, line_before))
                    current_linenum += 1
            sio.write('> %d | %s' % (self.linenum, self.line))
            if self.lines_after:
                current_linenum = self.linenum + 1
                for line_after in self.lines_after:
                    sio.write('  %d | %s' % (current_linenum, line_after))
                    current_linenum += 1
            else:
                sio.write('\n')
        else:
            if self.linenum and self.line:
                sio.write(': %d: %s' % (self.linenum, self.line.strip()))
        s = sio.getvalue()
        sio.close()
        return s

    def __unicode__(self):
        return self.__str__()

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

