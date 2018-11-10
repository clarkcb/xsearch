# -*- coding: utf-8 -*-
###############################################################################
#
# searchresult.py
#
# class SearchResult: encapsulates a search result
#
###############################################################################
from io import StringIO


def __atmost_before_index(s, maxlen, start_index):
    if start_index >= maxlen:
        return '...' + s[start_index - maxlen - 3:start_index]
    else:
        return s[:start_index]


def __atmost_after_index(s, maxlen, start_index):
    if len(s[start_index:]) > maxlen:
        return s[start_index:maxlen - 3] + '...'
    else:
        return s[start_index:]


def strip_newlines(s):
    return s.rstrip("\r\n")


class SearchResult(object):
    """encapsulates a search result"""
    SEPARATOR_LEN = 80

    def __init__(self, **kargs):
        self.pattern = ''
        self.file = None
        self.linenum = 0
        self.line = ''
        self.contained = ''
        self.lines_before = []
        self.lines_after = []
        self.match_start_index = 0
        self.match_end_index = 0
        self.maxlinelength = 150
        self.__dict__.update(kargs)

    def sortkey(self):
        path = self.file.path.lower()
        filename = self.file.filename.lower()
        return path, filename, self.linenum, self.match_start_index

    def __str__(self):
        if self.lines_before or self.lines_after:
            return self.__multiline_str()
        else:
            return self.__singleline_str()

    def __singleline_str(self):
        sio = StringIO()
        sio.write(str(self.file))
        if self.linenum and self.line:
            sio.write(': {0}: [{1}:{2}]'.format(self.linenum,
                      self.match_start_index, self.match_end_index))
            sio.write(': {0}'.format(self.__format_matching_line()))
        else:
            sio.write(' matches at [{0}:{1}]'.format(self.match_start_index, self.match_end_index))
        s = sio.getvalue()
        sio.close()
        return s

    def __format_line(self, line):
        formatted = self.__atmost_after_index(line, self.maxlinelength, 0).strip()
        return formatted

    def __format_matching_line(self):
        formatted = self.line
        linelength = len(self.line)
        matchlength = self.match_end_index - self.match_start_index
        if linelength > self.maxlinelength:
            adjusted_maxlength = self.maxlinelength - matchlength
            before_index = self.match_start_index
            if self.match_start_index > 0:
                before_index = before_index - (adjusted_maxlength / 4)
                if before_index < 0:
                    before_index = 0
            adjusted_maxlength = adjusted_maxlength - (self.match_start_index - before_index)
            after_index = self.match_end_index + adjusted_maxlength
            if after_index > linelength:
                after_index = linelength
            before = ''
            if before_index > 3:
                before = '...'
                before_index += 3
            after = ''
            if after_index < linelength - 3:
                after = '...'
                after_index -= 3
            formatted = before + self.line[before_index:after_index] + after
        return formatted.strip()

    def linenum_padding(self):
        max_linenum = self.linenum + len(self.lines_after)
        return len(str(max_linenum))

    def __multiline_str(self):
        sio = StringIO()
        sio.write('{0}\n{1}:'.format('=' * self.SEPARATOR_LEN, str(self.file)))
        sio.write(' {0}: [{1}:{2}]'.format(self.linenum, self.match_start_index,
                                           self.match_end_index))
        if self.contained:
            sio.write(': {0}'.format(self.contained))
        sio.write('\n{0}\n'.format('-' * self.SEPARATOR_LEN))
        line_format = ' {0:>' + str(self.linenum_padding()) + '} | {1}\n'
        current_linenum = self.linenum
        if self.lines_before:
            current_linenum -= len(self.lines_before)
            for line_before in self.lines_before:
                sio.write(' ' + line_format.format(current_linenum,
                                                   strip_newlines(line_before)))
                current_linenum += 1
        sio.write('>' + line_format.format(self.linenum,
                                           strip_newlines(self.line)))
        if self.lines_after:
            current_linenum = self.linenum + 1
            for line_after in self.lines_after:
                sio.write(' ' + line_format.format(current_linenum,
                                                   strip_newlines(line_after)))
                current_linenum += 1
        s = sio.getvalue()
        sio.close()
        return s

    def __unicode__(self):
        return str(self.__str__())

    def __repr__(self):
        s = '<{0}'.format(self.__class__.__name__)
        s += ' pattern: "{0}"'.format(self.pattern)
        s += ', filename: "{0}"'.format(self.file)
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
