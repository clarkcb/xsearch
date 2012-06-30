################################################################################
#
# searcher.py
#
# class Searcher: executes a file search
#
################################################################################
from cStringIO import StringIO
from datetime import datetime
import os
import re

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

from fileutil import FileUtil
from searchresult import SearchResult

class Searcher:
    '''a class to search files'''

    DEFAULT_DIR_FILTER_REGEXES = [re.compile(d) for d in (r'^CVS$', r'^\.git$', r'^\.svn$')]
    DEFAULT_FILE_FILTER_REGEXES = [re.compile(f) for f in (r'^\.DS_Store$',)]

    def __init__(self, searchsettings, **kargs):
        self.fileutil = FileUtil()
        self.settings = searchsettings
        self.results = []
        self.patterndict = {}
        self.timers = {}
        self.filedict = {}
        self.rescounts = {}
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
        if self.settings.verbose:
            print 'Files to be searched:'
            for f in searchfiles:
                print f
            print
        if self.settings.dotiming:
            self.start_timer('search_files')
        for f in searchfiles:
            self.search_file(f)
        if self.settings.dotiming:
            self.stop_timer('search_files')
        if self.settings.printresults:
            print
            self.print_res_counts()

        if self.settings.listfiles:
            file_list = self.get_matching_files()
            if file_list:
                print '\nFiles with matches:'
                for f in file_list:
                    print f

        if self.settings.listlines:
            line_list = self.get_matching_lines()
            if line_list:
                print '\nLines with matches:'
                for line in line_list:
                    print line


    def search_file(self, f):
        '''Search in a file, return number of matches found'''
        if not self.fileutil.is_searchable_file(f):
            if self.settings.verbose or self.settings.debug:
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
                    if self.settings.numlinesafter:
                        while len(lines_after) < self.settings.numlinesafter:
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
            if self.settings.numlinesbefore:
                if len(lines_before) == self.settings.numlinesbefore:
                    lines_before.pop(0)
                if len(lines_before) < self.settings.numlinesbefore:
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
            self.print_result(search_result)


    def print_result(self, search_result):
        '''Print the current search result info to the console'''
        sio = StringIO()
        if len(self.settings.searchpatterns) > 1:
            sio.write('%s: ' % search_result.pattern)
        sio.write(str(search_result))
        s = sio.getvalue()
        sio.close()
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


    def get_matching_files(self, pattern=None):
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


    def get_matching_lines(self, pattern=None):
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
