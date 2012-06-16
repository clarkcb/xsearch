#!/usr/bin/env python
################################################################################
#
# com.eLocale.search
#
# Class Searcher: search files
#
################################################################################

__version__ = "$Revision: $"
# $Source$

from datetime import datetime
from pprint import pprint
import os
import re
import sys
from cStringIO import StringIO

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

from com.eLocale.file import File

DEBUG = True

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
        self.printresults = False
        self.re_search_set = set()
        self.searchcompressed = True
        self.startpath = None
        self.dotiming = False
        self.verbose = False
        self._searchpatterns.extend(self.cs_searchpatterns)
        self._searchpatterns.extend(self.ci_searchpatterns)
        self.re_search_set.update([re.compile(s, re.S) for s in self.cs_searchpatterns])
        self.re_search_set.update([re.compile(s, re.I | re.S) for s in self.ci_searchpatterns])

    @property
    def searchpatterns(self):
        return self._searchpatterns

    def __str__(self):
        s = 'SearchSettings(startpath: "%s"' % self.startpath
        if self.in_extensions:
            s += ", in_extensions: %s" % str(self.in_extensions)
        if self.out_extensions:
            s+= ", out_extensions: %s" % str(self.out_extensions)
        if self.in_dirpatterns:
            s += ", in_dirpatterns: %s" % str(self.in_dirpatterns)
        if self.out_dirpatterns:
            s+= ", out_dirpatterns: %s" % str(self.out_dirpatterns)
        if self.in_filepatterns:
            s += ", in_filepatterns: %s" % str(self.in_filepatterns)
        if self.out_filepatterns:
            s+= ", out_filepatterns: %s" % str(self.out_filepatterns)
        if self.cs_searchpatterns:
            s+= ", cs_searchpatterns: %s" % str(self.cs_searchpatterns)
        if self.ci_searchpatterns:
            s+= ", ci_searchpatterns: %s" % str(self.ci_searchpatterns)
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

    def is_binary_file(self, f):
        '''Return true if file is of a (known) searchable binary file type'''
        ext = File.get_extension(f).lower()
        return (ext in self.BINARY_EXTS)

    def is_compressed_file(self, f):
        '''Return true if file is of a (known) compressed file type'''
        ext = File.get_extension(f).lower()
        return (ext in self.COMPRESSED_EXTS)

    def is_searchable_file(self, f):
        '''Return true if file is of a (known) searchable type'''
        ext = File.get_extension(f).lower()
        return (ext in self.SEARCHABLE_EXTS)

    def is_text_file(self, f):
        '''Return true if file is of a (known) text file type'''
        ext = File.get_extension(f).lower()
        return (ext in self.TEXT_EXTS)


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
            file_filter_predicates.append(lambda f: File.get_extension(f) in self.settings.in_extensions)
        if self.settings.out_extensions:
            file_filter_predicates.append(lambda f: not File.get_extension(f) in self.settings.out_extensions)
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
        searchfilelist = []
        for root, dirs, files in os.walk(self.settings.startpath):
            searchfilelist.extend([
                os.path.join(root,f) for f in files \
                if self.is_target_file(os.path.join(root,f))])
        return searchfilelist

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
        searchfilelist = self.get_search_files()
        if self.settings.dotiming:
            self.stop_timer('get_search_files')
        #print 'searchfilelist: %s' % str(searchfilelist)
        if self.settings.dotiming:
            self.start_timer('search_files')
        for f in searchfilelist:
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
        fo = File.get_file(f, 'r')
        if fo:
            results = self.search_text_file_obj(fo, f)
            fo.close()
            matchesfound = len(results)
            for pattern in results:
                for search_result in results[pattern]:
                    #print '%s:%d:%s' % (os.path.join(path, f),linenum, line)
                    self.add_search_result(search_result)
        return matchesfound

    def search_text_file_obj(self, fo, filename=''):
        '''Search in a given text file object and return the results'''
        if self.settings.multilinesearch:
            return self.search_text_file_obj_contents(fo, filename)
        else:
            return self.search_text_file_obj_lines(fo, filename)

    def search_text_file_obj_contents(self, fo, filename=''):
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
                if s in results:
                    results[s].append(search_result)
                else:
                    results[s] = [search_result]
        return results


    def search_text_file_obj_lines(self, fo, filename=''):
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
            if DEBUG:
                print 'searching %s file: %s' % (ext, f) 
            try:
                matchesfound = self.search_zip_file(f)
            except zipfile.BadZipfile, e:
                if not ext == 'ear':
                    print 'BadZipfile: %s: %s' % (str(e), f)
        elif ext in ('bz2', 'tar', 'tgz', 'gz') and tarfile.is_tarfile(f):
            # handle tar files
            if DEBUG:
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
            if DEBUG: print 'zipinfo.filename: %s' % zipinfo.filename
            if zipinfo.file_size and not self.filter_file(zipinfo.filename):
                if DEBUG: print 'file_size and not filter_file: %s' % zipinfo.filename
                if self.is_text_file(zipinfo.filename):
                    if DEBUG: print 'searchable text file in zip: %s' % zipinfo.filename
                    sio = StringIO(z.read(zipinfo.filename))
                    sio.seek(0)
                    results = self.search_text_file_obj(sio, f, zipinfo.filename)
                    matchesfound = len(results)
                    for pattern in results:
                        for search_result in results[pattern]:
                            #print '%s:%d:%s' % (f, linenum, line)
                            self.add_search_result(search_result)
                elif self.is_searchable_file(zipinfo.filename):
                    if DEBUG: print 'searchable binary file in zip: %s' % zipinfo.filename
        return matchesfound


    def search_tar_file(self, f, ext):
        '''Search a tar file, return number of matches found'''
        matchesfound = 0
        try:
            tar = tarfile.open(f, 'r:'+ext)
            for tarinfo in tar:
                if tarinfo.isreg() and not self.filter_file(tarinfo.name):
                    if DEBUG: print 'isreg and not filter_file: %s' % tarinfo.name
                    if self.is_text_file(tarinfo.name):
                        if DEBUG: print 'searchable text file in tar: %s' % tarinfo.name
                        tf = tar.extractfile(tarinfo)
                        results = self.search_text_file_obj(tf, f, tarinfo.name)
                        matchesfound = len(results)
                        for pattern in results:
                            for search_result in results[pattern]:
                                self.add_search_result(search_result)
                    elif  self.is_searchable_file(tarinfo.name):
                        if DEBUG: print 'searchable binary file in tar: %s' % tarinfo.name
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
        if len(self.searchpatterns) > 1:
            s += '%s: ' % search_result.pattern
        s += '%s' % os.path.join(search_result.path, search_result.filename)
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


def usage():
    print 'Usage:'
    print '''%s [options] <startpath>

options:
  -1                             show only the first match for a file+search combination
  --firstmatch                   (used to identify files with matches)

  -a                             show all matches
  --allmatches                   (the default)

  -b                             number of lines to show before every match
  --numlinesbefore               (default is 0)

  -B                             number of lines to show after every match
  --numlinesafter                (default is 0)

  -c                             case-sensitive searches
  --casesensitive                (applies to all searches; the default)

  -C                             case-insensitive searches
  --ignorecase                   (applies to all searches)

  -d <pattern>                   0+ dir name patterns for dirs to be searched
  --dirname=<pattern>            (default is all dirs)

  -D <pattern>                   0+ dir name patterns for dirs to be skipped
  --dirfilter=<pattern>          (default is no dirs filtered)

  -f <pattern>                   0+ file name patterns for files to be searched
  --filename=<pattern>           (default is all files)

  -F <pattern>                   0+ file name patterns for files to be skipped
  --filefilter=<pattern>         (default is no files filtered)

  --ignorecasesearchfile=<file>  path to text file containing case-insensitive search strings
                                 (one per line)

  --linelist                     generate a list of the matching lines after searching

  --linesafterfilter             a pattern to filter the "lines-after" lines on
                                 (used with --numlinesafter)

  --linesaftersearch             a pattern to search the "lines-after" lines on
                                 (used with --numlinesafter)

  --linesbeforefilter            a pattern to filter the "lines-before" lines on
                                 (used with --numlinesbefore)

  --linesbeforesearch            a pattern to search the "lines-before" lines on
                                 (used with --numlinesbefore)

  --listfiles                    generate a list of the matching files after searching

  -p                             print matches to stdout as found
  --printmatches                 (the default)

  -P                             suppress printing of matches to stdout
  --noprintmatches               

  -s <search>                    a case-sensitive search pattern
  --search=<search>              (one or more, compiled as regex)

  -S <search>                    a case-insensitive search pattern
  --ignorecasesearch=<search>    (zero or more, compiled as regex)

  --searchfile=<file>            path to text file containing search strings
                                 (one per line)

  -x <ext>                       0+ file extensions for file types to be searched
  --ext=<ext>                    (zero or more, comma-separated, default is *)

  -X <ext>                       0+ file extensions for file types to be skipped
  --extfilter=<ext>              (zero or more, comma-separated)

  -v                             be verbose
  --verbose

  -z                             search compressed files (tar, bz2, gz, zip)
  --searchcompressed             (the default)

  -Z                             do not search compressed files
  --nosearchcompressed           (tar, bz2, gz, zip)
''' % os.path.basename(sys.argv[0])
    sys.exit(1)


def search_settings_from_args(args):
    # getopts
    import getopt
    shortargs = 'ab:B:1d:D:f:F:mn:N:pPs:S:tvVx:X:zZ'
    longargs = '''allmatches dirfilter= dirname= dotiming ext= extfilter=
                  filefilter= filename= firstmatch
                  ignorecasesearch= ignorecasesearchfile=
                  linesafterfilter= linesaftersearch=
                  linesbeforefilter= linesbeforesearch= listfiles listlines
                  multilinesearch name= noprintresults nosearchcompressed
                  numlinesafter= numlinesbefore= printresults
                  search= searchcompressed searchfile= verbose version'''.split()

    try:
        opts, args = getopt.getopt(args, shortargs, longargs)
    except getopt.GetoptError:
        usage()

    if not opts and not args:
        usage()

    settings = SearchSettings()

    for o, a in opts:
        if o in ('-a', '--allmatches'):
            settings.firstmatch = False
        elif o in ('-b', '--numlinesbefore'):
            settings.numlinesbefore = int(a)
        elif o in ('-B', '--numlinesafter'):
            settings.numlinesafter = int(a)
        elif o in ('-1', '--firstmatch'):
            settings.firstmatch = True
        elif o in ('-c', '--casesensitive'):
            settings.ignorecase = False
        elif o in ('-C', '--ignorecase'):
            settings.ignorecase = True
        elif o in ('-d', '--dirname'):
            settings.in_dirpatterns.add(a)
        elif o in ('-D', '--dirfilter'):
            settings.out_dirpatterns.add(a)
        elif o in ('-f', '--filename'):
            settings.in_filepatterns.add(a)
        elif o in ('-F', '--filefilter'):
            settings.out_filepatterns.add(a)
        elif o == '--listfiles':
            settings.listfiles = True
        elif o == '--listlines':
            settings.listlines = True
        elif o in ('-m', '--multilinesearch'):
            settings.multilinesearch = True
        elif o in ('-p', '--printresults'):
            settings.printresults = True
        elif o in ('-P', '--noprintresults'):
            settings.printresults = False
        elif o in ('-s', '--search'):
            settings.cs_searchpatterns.add(a)
        elif o in ('-S', '--ignorecasesearch'):
            settings.ci_searchpatterns.add(a)
        elif o in ('-t', '--dotiming'):
            settings.dotiming = True
        elif o in ('-x', '--ext'):
            settings.in_extensions.add(a)
        elif o in ('-X', '--extfilter'):
            settings.out_extensions.add(a)
        elif o in ('-z', '--searchcompressed'):
            settings.searchcompressed = True
        elif o in ('-Z', '--nosearchcompressed'):
            settings.searchcompressed = False
        elif o == '--linesafterfilter':
            settings.linesafterfilters.append(a)
        elif o == '--linesbeforefilter':
            settings.linesbeforefilters.append(a)
        elif o == '--linesaftersearch':
            settings.linesaftersearches.append(a)
        elif o == '--linesbeforesearch':
            settings.linesbeforesearches.append(a)
        elif o in ('-v', '--verbose'):
            settings.verbose = True
        elif o in ('-V', '--version'):
            print VERSION
            sys.exit(0)
        else:
            print 'o: %s' % str(o)
            print 'a: %s' % str(a)
            usage()

    if args:
        settings.startpath = args[0]
    else:
        settings.startpath = '.'

    return settings


def main():
    if DEBUG:
        print 'sys.argv(%d): %s' % (len(sys.argv),str(sys.argv))

    if len(sys.argv) < 4:
        usage()

    searchsettings = search_settings_from_args(sys.argv[1:])

    if DEBUG:
        print 'searchsettings: %s' % str(searchsettings)

    try:
        searcher = Searcher(searchsettings)
        searcher.search()
    except KeyboardInterrupt:
        print
        sys.exit(0)

    print
    searcher.print_res_counts()

    if searchsettings.printresults or DEBUG:
        print '\nsearch results:'
        pprint(searcher.results)


        if searchsettings.listfiles:
            print '\nfiles with results:'
            file_list = searcher.get_file_list()
            for f in file_list:
                print f

        if searchsettings.listlines:
            print '\nlines with results:'
            line_list = searcher.get_line_list()
            for line in line_list:
                print line


if __name__ == '__main__':
    main()
