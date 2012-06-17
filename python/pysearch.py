#!/usr/bin/env python
################################################################################
#
# pysearch.py
#
# A file search utility implemented in python (2.x)
#
################################################################################

__version__ = "$Revision: $"
# $Source$

from cStringIO import StringIO
from datetime import datetime
import os
from pprint import pprint
import re
import sys

TARFILE_MODULE_AVAILABLE = True
ZIPFILE_MODULE_AVAILABLE = True

try:
    import tarfile
except ImportError, e:
    print 'tarfile not imported: %s' % e
    TARFILE_MODULE_AVAILABLE = False
try:
    import zipfile
except ImportError, e:
    print 'zipfile not imported: %s' % e
    ZIPFILE_MODULE_AVAILABLE = False

DEBUG = False

class SearchOption:
    '''a class to encapsulate a specific command line option'''
    def __init__(self, shortarg, longarg, func, desc):
        self.shortarg = shortarg
        self.longarg = longarg
        self.func = func
        self.desc = desc

class SearchSettings:
    '''a class to encapsulate search settings for a particular search session'''
    def __init__(self):
        self._searchpatterns = []
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
        self.verbose = False

    def add_searchpattern(self, pattern, casesensitive=True):
        if casesensitive:
            self.cs_searchpatterns.add(pattern)
            self.re_search_set.add(re.compile(pattern, re.S))
        else:
            self.ci_searchpatterns.add(pattern)
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

class FileUtil:
    '''a file helper class'''

    # file types
    # for more info see /etc/httpd/mime.types
    NOSEARCH_EXTS   = '''aif aifc aiff au avi bmp cab dmg eps gif
                         ico idlk ief iso jpe jpeg jpg
                         m3u m4a m4p mov movie mp3 mp4 mpe mpeg mpg mxu
                         ogg pdf pict png ps qt ra ram rm rpm
                         scc snd suo tif tiff wav'''.split()
    BINARY_EXTS     = '''ai bin class com dat dbmdl dcr dir dll dxr dms doc docx dot exe
                         hlp indd lnk mo obj pdb ppt psd pyc pyo qxd so swf sys
                         vsd xls xlsx xlt'''.split()
    COMPRESSED_EXTS = '''bz2 cpio ear gz hqx jar pax rar sit sitx tar tgz
                         war zip Z'''.split()
    TEXT_EXTS       = '''1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
                         am app as asc ascx asm asp aspx bash bat bdsproj bsh
                         c cc cfg clj cls cmd cnt conf config cpp cs csh csproj css csv ctl
                         dat dbproj dbml dbschema ddl dep dfm disco dlg dof dpr dsp dsw dtd
                         env etx exp fls fs fsproj h hpp htm html ics iml in inc ini ipr iws
                         java js jsp layout log mak map master mht mxml
                         pas php php3 pl plist pm po properties py
                         rc rc2 rdf resx rex rtf rtx
                         scc sgm sgml sh sln smi smil spec sqc sql st str strings
                         suml svg sxw
                         t tcl tld tmx tsv txt url user
                         vb vbproj vbs vcf vcproj vdproj vm vrml vssscc vxml
                         wbxml webinfo wml wmls wrl wsd wsdd wsdl
                         xlf xml xsd xsl xslt'''.split()
    UNKNOWN_EXTS    = '''adm aps cli clw dat db def df2 ncb nt nt2 orig
                         pc plg roff sun t tex texinfo tr xwd'''.split()
    SEARCHABLE_EXTS = BINARY_EXTS + COMPRESSED_EXTS + TEXT_EXTS

    def get_extension(self, filename):
        '''Returns the extension for a given filename, if any, else empty string'''
        ext = ''
        if os.path.basename(filename).rfind('.') > 0:
            ext = filename.split('.')[-1]
        return ext.lower()

    def is_binary_file(self, f):
        '''Return true if file is of a (known) searchable binary file type'''
        return (self.get_extension(f) in self.BINARY_EXTS)

    def is_compressed_file(self, f):
        '''Return true if file is of a (known) compressed file type'''
        return (self.get_extension(f) in self.COMPRESSED_EXTS)

    def is_searchable_file(self, f):
        '''Return true if file is of a (known) searchable type'''
        return (self.get_extension(f) in self.SEARCHABLE_EXTS)

    def is_text_file(self, f):
        '''Return true if file is of a (known) text file type'''
        return (self.get_extension(f) in self.TEXT_EXTS)


class Searcher:
    '''a class to search files'''

    DEFAULT_DIR_FILTER_REGEXES = [re.compile(d) for d in [r'^CVS$',r'^\.svn$']]
    DEFAULT_FILE_FILTER_REGEXES = [re.compile(f) for f in [r'^\.DS_Store$']]

    def __init__(self, searchsettings, **kargs):
        self.fileutil = FileUtil()
        self.settings = searchsettings
        self.results = []
        self.patterndict = {}
        self.timers = {}
        self.filedict = {}
        self.rescounts = {}
        self.numlinesbefore = 0
        self.numlinesafter = 0
        self.linesbeforefilters = []
        self.linesafterfilters = []
        self.linesbeforesearches = []
        self.linesaftersearches = []
        self.__dict__.update(kargs)
        self.file_filter_predicates = self.get_file_filter_predicates()

    def matches_any_pattern(self, s, pattern_set):
        '''Returns true if string matches any pattern in pattern_set, else false'''
        for p in pattern_set:
            p = re.compile(p)
            if p.search(s):
                return True
        return False

    def get_file_filter_predicates(self):
        file_filter_predicates = []
        if self.settings.in_extensions:
            file_filter_predicates.append(lambda f: self.fileutil.get_extension(f) in self.settings.in_extensions)
        if self.settings.out_extensions:
            file_filter_predicates.append(lambda f: not self.fileutil.get_extension(f) in self.settings.out_extensions)
        if self.settings.in_dirpatterns:
            file_filter_predicates.append(lambda f: self.matches_any_pattern(os.path.dirname(f), self.settings.in_dirpatterns))
        if self.settings.out_dirpatterns:
            file_filter_predicates.append(lambda f: not self.matches_any_pattern(os.path.dirname(f), self.settings.out_dirpatterns))
        if self.settings.in_filepatterns:
            file_filter_predicates.append(lambda f: self.matches_any_pattern(os.path.basename(f), self.settings.in_filepatterns))
        if self.settings.out_filepatterns:
            file_filter_predicates.append(lambda f: not self.matches_any_pattern(os.path.basename(f), self.settings.out_filepatterns))
        return file_filter_predicates

    def is_target_file(self, f):
        for pred in self.file_filter_predicates:
            if not pred(f):
                return False
        return True

    def get_search_files(self):
        '''Get the list of files to search'''
        searchfiles = []
        for root, dirs, files in os.walk(self.settings.startpath):
            searchfiles.extend([
                os.path.join(root,f) for f in files \
                if self.is_target_file(os.path.join(root,f))])
        return searchfiles

    def start_timer(self, name):
        start = datetime.now()
        self.timers[name+':start'] = start

    def stop_timer(self, name):
        start = self.timers[name+':start']
        stop = datetime.now()
        self.timers[name+':stop'] = stop
        elapsed = stop - start
        self.timers[name+':elapsed'] = elapsed
        print 'Elapsed time for %s: %s' % (name, elapsed)

    def search(self):
        '''Search files to find instances of searchpattern(s) starting from startpath'''
        if self.settings.dotiming:
            self.start_timer('get_search_files')
        searchfiles = self.get_search_files()
        if self.settings.dotiming:
            self.stop_timer('get_search_files')
        #print 'searchfiles: %s' % str(searchfiles)
        if self.settings.dotiming:
            self.start_timer('search_files')
        for f in searchfiles:
            self.search_file(f)
        if self.settings.dotiming:
            self.stop_timer('search_files')

    def search_file(self, f):
        '''Search in a file, return number of matches found'''
        if not self.fileutil.is_searchable_file(f):
            if self.verbose or DEBUG:
                print 'Skipping unsearchable file: %s' % f
                return 0
        matchesfound = 0
        if self.fileutil.is_text_file(f):
            matchesfound = self.search_text_file(f)
        elif self.fileutil.is_compressed_file(f) and self.searchcompressed:
            try:
                matchesfound = self.search_compressed_file(f)
            except IOError, e:
                print 'IOError: %s: %s' % (str(e), f)
        return matchesfound

    def search_text_file(self, f, enc=None):
        '''Search a text file, return number of matches found'''
        matchesfound = 0
        try:
            fo = open(f, 'r')
            results = {}
            if self.settings.multilinesearch:
                results = self.search_text_file_contents(fo, f)
            else:
                results = self.search_text_file_lines(fo, f)
            fo.close()
            matchesfound = len(results)
        except IOError, e:
            print 'IOError: %s: %s' % (str(e), f)

        return matchesfound

    def search_text_file_contents(self, fo, filename=''):
        '''Search in a given text file object contents (all at once)
           and return the results
        '''
        results  = {}
        contents = fo.read()
        for s in self.settings.searchpatterns:
            if s in results and self.settings.firstmatch:
                continue
            matches = s.finditer(contents)
            for m in matches:
                #print "match: m: %s, m.group(): %s" % (m, m.group())
                before_line_count = len(re.findall(r'(\r\n|\n)', m.string[0:m.start()]))
                after_line_count = len(re.findall(r'(\r\n|\n)', m.string[m.end():]))
                line_start_index = m.start()
                line_end_index = m.end()
                if before_line_count:
                    line_start_index = m.string[0:m.start()].rfind('\n')
                else:
                    line_start_index = 0
                if after_line_count:
                    line_end_index = m.string.find('\n', m.end())
                else:
                    line_end_index = len(m.string())-1
                #print 'line_start_index: %d' % line_start_index
                #print 'line_end_index: %d' % line_end_index
                line = m.string[line_start_index:line_end_index]
                search_result = SearchResult(pattern=s.pattern,
                                             filename=filename,
                                             linenum=before_line_count+1,
                                             line=line)
                self.add_search_result(search_result)
                if s in results:
                    results[s].append(search_result)
                else:
                    results[s] = [search_result]
        return results


    def search_text_file_lines(self, fo, filename=''):
        '''Search in a given text file object by line and return the results'''
        results  = {}
        linenum = 0
        lines_before = []
        lines_after = []
        while True:
            if lines_after:
                line = lines_after.pop(0)
            else:
                try:
                    line = fo.next()
                except StopIteration, e:
                    break
                except AttributeError, e:
                    #print 'AttributeError: %s' % e
                    break
            linenum += 1
            for s in self.settings.re_search_set:
                if s in results and self.settings.firstmatch:
                    continue
                if s.search(line):
                    if self.numlinesafter:
                        while len(lines_after) < self.numlinesafter:
                            try:
                                lines_after.append(fo.next())
                            except StopIteration, e:
                                break
                    search_result = SearchResult(pattern=s.pattern, filename=filename, linenum=linenum,
                                                 line=line, lines_before=lines_before[:], lines_after=lines_after[:])
                    if self.settings.numlinesbefore and lines_before:
                        if self.settings.linesbeforefilters:
                            lines_before_filter_match = False
                            for f in self.settings.linesbeforefilters:
                                for l in lines_before:
                                    if self.verbose or DEBUG:
                                        print 'checking line for linesbeforefilter "%s": %s' % (f.pattern, l.strip())
                                    if f.search(l):
                                        if self.verbose or DEBUG:
                                            print 'found line before matching linesbeforefilter "%s": %s' % (f.pattern, l.strip())
                                        lines_before_filter_match = True
                                        break
                                if lines_before_filter_match:
                                    search_result = None
                                    break
                    if search_result and self.settings.numlinesafter and lines_after:
                        if self.settings.linesafterfilters:
                            lines_after_filter_match = False
                            for f in self.settings.linesafterfilters:
                                for l in lines_after:
                                    if self.verbose or DEBUG:
                                        print 'checking line for linesafter filter "%s": %s' % (f.pattern, l.strip())
                                    if f.search(l):
                                        if self.verbose or DEBUG:
                                            print 'found line after matching linesafterfilter "%s": %s' % (f.pattern, l.strip())
                                        lines_after_filter_match = True
                                        break
                                if lines_after_filter_match:
                                    search_result = None
                                    break
                    if search_result:
                        self.add_search_result(search_result)
                        if s in results:
                            results[s].append(search_result)
                        else:
                            results[s] = [search_result]
            if self.numlinesbefore:
                if len(lines_before) == self.numlinesbefore:
                    lines_before.pop(0)
                if len(lines_before) < self.numlinesbefore:
                    lines_before.append(line)
        return results


    def search_compressed_file(self, f):
        '''Search a compressed file, return number of matches found'''
        matchesfound = 0
        ext = File.get_extension(f)
        if not ext: return
        ext = ext.lower()
        if ext in ('zip', 'jar', 'war', 'ear'):
            # handle zip files
            if self.settings.debug:
                print 'searching %s file: %s' % (ext, f) 
            try:
                matchesfound = self.search_zip_file(f)
            except zipfile.BadZipfile, e:
                if not ext == 'ear':
                    print 'BadZipfile: %s: %s' % (str(e), f)
        elif ext in ('bz2', 'tar', 'tgz', 'gz') and tarfile.is_tarfile(f):
            # handle tar files
            if self.settings.debug:
                print 'searching %s file: %s' % (ext, f)
            try:
                matchesfound = self.search_tar_file(f, ext)
            except Exception, e:
                print 'Exception while searching a tar file %s: %s' % (f, str(e))
        return matchesfound


    def search_zip_file(self, f):
        '''Search a jar/zip file, return number of matches found'''
        matchesfound = 0
        z = zipfile.ZipFile(f, 'r')
        zipinfos = z.infolist()
        for zipinfo in zipinfos:
            if self.settings.debug: print 'zipinfo.filename: %s' % zipinfo.filename
            if zipinfo.file_size and not self.filter_file(zipinfo.filename):
                if self.settings.debug: print 'file_size and not filter_file: %s' % zipinfo.filename
                if self.is_text_file(zipinfo.filename):
                    if self.settings.debug: print 'searchable text file in zip: %s' % zipinfo.filename
                    sio = StringIO(z.read(zipinfo.filename))
                    sio.seek(0)
                    results = self.search_text_file_obj(sio, f, zipinfo.filename)
                    matchesfound = len(results)
                    for pattern in results:
                        for search_result in results[pattern]:
                            #print '%s:%d:%s' % (f, linenum, line)
                            self.add_search_result(search_result)
                elif self.is_searchable_file(zipinfo.filename):
                    if self.settings.debug: print 'searchable binary file in zip: %s' % zipinfo.filename
        return matchesfound


    def search_tar_file(self, f, ext):
        '''Search a tar file, return number of matches found'''
        matchesfound = 0
        try:
            tar = tarfile.open(f, 'r:'+ext)
            for tarinfo in tar:
                if tarinfo.isreg() and not self.filter_file(tarinfo.name):
                    if self.settings.debug: print 'isreg and not filter_file: %s' % tarinfo.name
                    if self.is_text_file(tarinfo.name):
                        if self.settings.debug: print 'searchable text file in tar: %s' % tarinfo.name
                        tf = tar.extractfile(tarinfo)
                        results = self.search_text_file_obj(tf, f, tarinfo.name)
                        matchesfound = len(results)
                        for pattern in results:
                            for search_result in results[pattern]:
                                self.add_search_result(search_result)
                    elif  self.is_searchable_file(tarinfo.name):
                        if self.settings.debug: print 'searchable binary file in tar: %s' % tarinfo.name
            tar.close()
        except tarfile.CompressionError, e:
            if not ext == 'tgz':
                print 'tarfile.CompressionError while trying to open %s: %s' % (f, str(e))
        return matchesfound


    def add_search_result(self, search_result):
        '''Add to list of search results'''
        self.results.append(search_result)
        pattern = search_result.pattern
        fullfile = os.path.abspath(search_result.filename)
        self.rescounts[pattern] = self.rescounts.setdefault(pattern, 0) + 1
        self.patterndict.setdefault(pattern, list()).append(search_result)
        self.filedict.setdefault(fullfile, list()).append(search_result)
        if self.settings.printresults:
            if search_result.lines_before or search_result.lines_after:
                print '%s' % ('=' * 80)
            self.print_result(search_result)


    def print_result(self, search_result):
        '''Print the current search result info to the console'''
        s = ''
        if len(self.settings.searchpatterns) > 1:
            s += '%s: ' % search_result.pattern
        #s += '%s' % os.path.join(search_result.path, search_result.filename)
        s += '%s' % search_result.filename
        if search_result.contained:
            s += ': %s' % search_result.contained
        if search_result.lines_before or search_result.lines_after:
            s += '\n%s\n' % ('-' * 80)
            current_linenum = search_result.linenum
            if search_result.lines_before:
                current_linenum -= len(search_result.lines_before)
                for l in search_result.lines_before:
                    s += '  %d | %s' % (current_linenum, l)
                    current_linenum += 1
            s += '> %d | %s' % (search_result.linenum, search_result.line)
            if search_result.lines_after:
                current_linenum = search_result.linenum + 1
                for l in search_result.lines_after:
                    s += '  %d | %s' % (current_linenum, l)
                    current_linenum += 1
            else:
                s += '\n'
        else:
            if search_result.linenum and search_result.line:
                s += ': %d: %s' % (search_result.linenum, search_result.line.strip())
        try:
            print s
        except UnicodeEncodeError, e:
            print repr(s)


    def print_res_counts(self):
        '''Print result counts'''
        for p in self.patterndict:
            if p in self.rescounts:
                match = 'match'
                if self.rescounts[p] > 1:
                    match += 'es'
                print '%d %s for "%s"' % (self.rescounts[p], match, p)
            else:
                print '0 matches for "%s"' % (p)


    def get_file_list(self, pattern=None):
        '''Get list of files with matches for a given pattern (or all patterns if none given)'''
        patterns = []
        if pattern:
            patterns.append(pattern)
        else:
            patterns.extend(self.patterndict.keys())
        file_list = set()
        for p in patterns:
            file_list.update([os.path.abspath(r.filename) for r in self.patterndict[p]])
        file_list = list(file_list)
        file_list.sort()
        return file_list


    def get_line_list(self, pattern=None):
        '''Get list of lines with matches for a given pattern (or all patterns if none given)'''
        patterns = []
        if pattern:
            patterns.append(pattern)
        else:
            patterns.extend(self.patterndict.keys())
        line_list = set()
        for p in patterns:
            line_list.update([r.line.strip() for r in self.patterndict[p]])
        line_list = list(line_list)
        line_list.sort()
        return line_list



class SearchResult:
    '''contains a search result'''
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


arg_options = (
    SearchOption('b', 'numlinesbefore', lambda x, settings: settings.set_property('numlinesbefore', int(x)),
     'Number of lines to show before every match (default is 0)'),
    SearchOption('B', 'numlinesafter', lambda x, settings: settings.set_property('numlinesafter', int(x)),
     'Number of lines to show after every match (default is 0)'),
    SearchOption('d', 'dirname', lambda x, settings: settings.in_dirpatterns.add(x),
     'Specify name pattern for directories to include in search'),
    SearchOption('D', 'dirfilter', lambda x, settings: settings.out_dirpatterns.add(x),
     'Specify name pattern for directories to exclude from search'),
    SearchOption('f', 'filename', lambda x, settings: settings.in_filepatterns.add(x),
     'Specify name pattern for files to include in search'),
    SearchOption('F', 'filefilter', lambda x, settings: settings.out_filepatterns.add(x),
     'Specify name pattern for files to exclude from search'),
    SearchOption('', 'ignorecasesearchfile', lambda x, settings: settings.set_property('ignorecasesearchfile', x),
     'Specify file containing case-insensitive search patterns (one per line)'),
    SearchOption('', 'linesafterfilter', lambda x, settings: settings.linesafterfilters.append(x),
     'Specify pattern to filter the "lines-after" lines on (used with --numlinesafter)'),
    SearchOption('', 'linesaftersearch', lambda x, settings: settings.linesaftersearches.append(x),
     'Specify pattern to search the "lines-after" lines on (used with --numlinesafter)'),
    SearchOption('', 'linesbeforefilter', lambda x, settings: settings.linesbeforefilters.append(x),
     'Specify pattern to filter the "lines-before" lines on (used with --numlinesbefore)'),
    SearchOption('', 'linesbeforesearch', lambda x, settings: settings.linesbeforesearches.append(x),
     'Specify pattern to search the "lines-before" lines on (used with --numlinesbefore)'),
    SearchOption('s', 'search', lambda x, settings: settings.add_searchpattern(x),
     'Specify search pattern'),
    SearchOption('S', 'ignorecasesearch', lambda x, settings: settings.add_searchpattern(x, casesensitive=False),
     'Specify case-insensitive search pattern'),
    SearchOption('', 'searchfile', lambda x, settings: settings.set_property('searchfile', x),
     'Specify file containing search patterns (one per line)'),
    SearchOption('x', 'ext', lambda x, settings: settings.in_extensions.add(x),
     'Specify extension for files to include in search'),
    SearchOption('X', 'extfilter', lambda x, settings: settings.out_extensions.add(x),
     'Specify extension for files to exclude from search')
)
flag_options = (
    SearchOption('1', 'firstmatch', lambda settings: settings.set_property('firstmatch', True),
     'Capture only the first match for a file+search combination'),
    SearchOption('a', 'allmatches', lambda settings: settings.set_property('firstmatch', False),
     'Capture all matches*'),
    SearchOption('c', 'casesensitive', lambda settings: settings.set_property('ignorecase', False),
     'Do case-sensitive searching*'),
    SearchOption('C', 'ignorecase', lambda settings: settings.set_property('ignorecase', True),
     'Do case-insensitive searching'),
    SearchOption('', 'debug', lambda settings: settings.set_property('debug', True),
     'Set output mode to debug'),
    SearchOption('', 'listfiles', lambda settings: settings.set_property('listfiles', True),
     'Generate a list of the matching files after searching'),
    SearchOption('', 'listlines', lambda settings: settings.set_property('listlines', True),
     'Generate a list of the matching lines after searching'),
    SearchOption('m', 'multilinesearch', lambda settings: settings.set_property('multilinesearch', True),
     'Search files by line*'),
    SearchOption('p', 'printmatches', lambda settings: settings.set_property('printresults', True),
     'Print matches to stdout as found*'),
    SearchOption('P', 'noprintmatches', lambda settings: settings.set_property('printresults', False),
     'Suppress printing of matches to stdout'),
    SearchOption('t', 'dotiming', lambda settings: settings.set_property('dotiming', True),
     'Time search execution'),
    SearchOption('v', 'verbose', lambda settings: settings.set_property('verbose', True),
     'Specify verbose output'),
    SearchOption('z', 'searchcompressed', lambda settings: settings.set_property('searchcompressed', True),
     'Search compressed files (bz2, gz, tar, zip)*'),
    SearchOption('Z', 'nosearchcompressed', lambda settings: settings.set_property('searchcompressed', False),
     'Search compressed files (bz2, gz, tar, zip)')
)

def get_arg_maps():
    '''Returns arg_map and flag_map'''
    arg_dict = {}
    for o in arg_options:
        if o.shortarg:
            arg_dict[o.shortarg] = o
        arg_dict[o.longarg] = o
    flag_dict = {}
    for o in flag_options:
         if o.shortarg:
             flag_dict[o.shortarg] = o
         flag_dict[o.longarg] = o
    return arg_dict, flag_dict


def search_settings_from_args(args):
    arg_dict, flag_dict = get_arg_maps()
    settings = SearchSettings()

    while args:
        arg = args.pop(0)
        if False:
            print 'next arg: "%s"' % arg
        if arg.startswith('-'):
            while arg and arg.startswith('-'):
                arg = arg[1:]
            if arg in arg_dict:
                if False:
                    print '"%s" found in arg_dict' % arg
                if args:
                    argval = args.pop(0)
                    arg_dict[arg].func(argval, settings)
                else:
                    print 'Error: missing value for option %s' % arg
                    usage()
            elif arg in flag_dict:
                if False:
                    print '"%s" found in flag_dict' % arg
                flag_dict[arg].func(settings)
            elif arg in ('V', 'version'):
                print __version__
                sys.exit(0)
            else:
                print 'Error: unknown option: %s' % arg
        else:
            settings.startpath = arg

    if not settings.startpath:
        print 'Error: missing startpath'
        usage()

    return settings

def get_option_sort_elem(option):
    if option.shortarg:
        return option.shortarg.lower()
    else:
        return option.longarg.lower()

def usage():
    print 'Usage:'
    print '%s [options] <startpath>\n\noptions:' % os.path.basename(sys.argv[0])
    options = arg_options + flag_options
    opt_strings = []
    opt_descs = []
    longest = 0
    for opt in sorted(options, key=lambda opt: get_option_sort_elem(opt)):
        s = ''
        if opt.shortarg:
            s += '-%s,' % opt.shortarg
        s += '--%s' % opt.longarg
        if len(s) > longest:
            longest = len(s)
        opt_strings.append(s)
        opt_descs.append(opt.desc)
    format_string = ' %%-%ds  %%s' % longest
    for i,s in enumerate(opt_strings):
        print format_string % (s, opt_descs[i])
    sys.exit(1)


def main():
    if DEBUG:
        print 'sys.argv(%d): %s' % (len(sys.argv),str(sys.argv))

    if len(sys.argv) < 4:
        usage()

    settings = search_settings_from_args(sys.argv[1:])

    if DEBUG:
        print 'searchsettings: %s' % str(settings)

    try:
        searcher = Searcher(settings)
        searcher.search()
    except KeyboardInterrupt:
        print
        sys.exit(0)

    print
    searcher.print_res_counts()

    if settings.listfiles:
        file_list = searcher.get_file_list()
        if file_list:
            print '\nFiles with matches:'
            for f in file_list:
                print f

    if settings.listlines:
        line_list = searcher.get_line_list()
        if line_list:
            print '\nLines with matches:'
            for line in line_list:
                print line


if __name__ == '__main__':
    main()
