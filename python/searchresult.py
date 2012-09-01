################################################################################
#
# searchresult.py
#
# class SearchResult: encapsulates a search result
#
################################################################################
from cStringIO import StringIO

class SearchResult:
    """encapsulates a search result"""

    def __init__(self, **kargs):
        self.pattern = ''
        self.filename = ''
        self.linenum = 0
        self.line = ''
        self.contained = ''
        self.lines_before = []
        self.lines_after = []
        self.__dict__.update(kargs)

    def linenum_padding(self):
        max_linenum = self.linenum + len(self.lines_before) + len(self.lines_after)
        if max_linenum < 100: return 2
        elif max_linenum < 1000: return 3
        elif max_linenum < 10000: return 4
        elif max_linenum < 100000: return 5
        else: return 6

    def __multiline_str(self):
        sio = StringIO()
        sio.write('{0}\n{1}'.format('=' * 80, self.filename))
        if self.contained:
            sio.write(': {0}'.format(self.contained))
        sio.write('\n{0}\n'.format('-' * 80))
        line_format = ' {0:>' + str(self.linenum_padding()) + '} | {1}'
        current_linenum = self.linenum
        if self.lines_before:
            current_linenum -= len(self.lines_before)
            for line_before in self.lines_before:
                sio.write(' ' + line_format.format(current_linenum, line_before))
                current_linenum += 1
        sio.write('>' + line_format.format(self.linenum, self.line))
        if self.lines_after:
            current_linenum = self.linenum + 1
            for line_after in self.lines_after:
                sio.write(' ' + line_format.format(current_linenum, line_after))
                current_linenum += 1
        else:
            sio.write('\n')
        s = sio.getvalue()
        sio.close()
        return s

    def __singleline_str(self):
        sio = StringIO()
        sio.write(self.filename)
        if self.linenum and self.line:
            sio.write(': {0}: {1}'.format(self.linenum, self.line.strip()))
        s = sio.getvalue()
        sio.close()
        return s

    def __str__(self):
        if self.lines_before or self.lines_after:
            return self.__multiline_str()
        else:
            return self.__singleline_str()

    def __unicode__(self):
        return self.__str__()

    def __repr__(self):
        s = '<{0}'.format(self.__class__.__name__)
        s += ' pattern: "{0}"'.format(self.pattern)
        s += ', filename: "{0}"'.format(self.filename)
        s += ', linenum: {0}'.format(self.linenum)
        s += ', line: "{0}"'.format(self.line.replace("\n", "\\n"))
        if self.contained:
            s += ', contained: "{0}"'.format(self.contained)
        if self.lines_before:
            s += ', lines_before: {0}'.format(repr(self.lines_before))
        if self.lines_after:
            s += ', lines_after: {0}'.format(repr(self.lines_after))
        s += '>'
        return s
