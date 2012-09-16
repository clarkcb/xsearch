################################################################################
#
# searcher.py
#
# class Searcher: executes a file search
#
################################################################################
from collections import deque
from cStringIO import StringIO
from datetime import datetime
import os
import re

TARFILE_MODULE_AVAILABLE = True
ZIPFILE_MODULE_AVAILABLE = True

try:
    import tarfile
except ImportError as e:
    print 'tarfile not imported: {0!s}'.format(e)
    TARFILE_MODULE_AVAILABLE = False
try:
    import zipfile
except ImportError as e:
    print 'zipfile not imported: {0!s}'.format(e)
    ZIPFILE_MODULE_AVAILABLE = False

from fileutil import FileUtil
from searchresult import SearchResult

class Searcher:
    """a class to search files"""

    def __init__(self, settings, **kargs):
        self.settings = settings
        self.validate_settings()
        self.fileutil = FileUtil()
        self.results = []
        self.patterndict = {}
        self.timers = {}
        self.filedict = {}
        self.rescounts = {}
        self.__dict__.update(kargs)
        self.file_filter_predicates = self.get_file_filter_predicates()

    def validate_settings(self):
        assert self.settings.startpath, 'Startpath not defined'
        assert os.path.exists(self.settings.startpath), 'Startpath not found'
        assert self.settings.searchpatterns, 'No search patterns specified'

    def matches_any_pattern(self, s, pattern_set):
        """Returns true if string matches any pattern in pattern_set, else
           false"""
        for p in pattern_set:
            if p.search(s):
                return True
        return False

    def any_matches_any_pattern(self, slist, pattern_set):
        """Returns true if any string in slist matches any pattern in
        pattern_set, else false"""
        for s in slist:
            if self.matches_any_pattern(s, pattern_set):
                return True
        return False

    def get_ext(self, f):
        return self.fileutil.get_extension(f)

    def get_file_filter_predicates(self):
        predicate_definitions = [
            (self.settings.in_extensions,
             lambda f:
                self.get_ext(f) in self.settings.in_extensions),
            (self.settings.out_extensions,
             lambda f:
                not self.get_ext(f) in self.settings.out_extensions),

            (self.settings.in_dirpatterns,
             lambda f:
                self.matches_any_pattern(os.path.dirname(f),
                    self.settings.in_dirpatterns)),
            (self.settings.out_dirpatterns,
             lambda f:
                not self.matches_any_pattern(os.path.dirname(f),
                    self.settings.out_dirpatterns)),
            (self.settings.in_filepatterns,
             lambda f:
                self.matches_any_pattern(os.path.basename(f),
                    self.settings.in_filepatterns)),
            (self.settings.out_filepatterns,
             lambda f:
                not self.matches_any_pattern(os.path.basename(f),
                    self.settings.out_filepatterns)),
        ]
        return [p for (s,p) in predicate_definitions if s]

    def is_target_file(self, f):
        for pred in self.file_filter_predicates:
            if not pred(f):
                return False
        return True

    def get_search_files(self):
        """Get the list of files to search"""
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
        stop = datetime.now()
        self.timers[name+':stop'] = stop
        start = self.timers[name+':start']
        elapsed = stop - start
        self.timers[name+':elapsed'] = elapsed
        print 'Elapsed time for {0}: {1}'.format(name, elapsed)

    def search(self):
        """Search files to find instances of searchpattern(s) starting from
           startpath"""
        if self.settings.dotiming:
            self.start_timer('get_search_files')
        searchfiles = self.get_search_files()
        if self.settings.dotiming:
            self.stop_timer('get_search_files')
        if self.settings.verbose:
            print '\nFiles to be searched ({0}):'.format(len(searchfiles))
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
        """Search in a file, return number of matches found"""
        if not self.fileutil.is_searchable_file(f):
            if self.settings.verbose or self.settings.debug:
                print 'Skipping unsearchable file: {0}'.format(f)
            return
        if self.fileutil.is_text_file(f):
            self.search_text_file(f)
        elif self.fileutil.is_compressed_file(f) and self.searchcompressed:
            try:
                self.search_compressed_file(f)
            except IOError as e:
                print 'IOError: {0!s}: {1}'.format(e, f)

    def search_text_file(self, f, enc=None):
        """Search a text file, return number of matches found"""
        try:
            fo = open(f, 'r')
            results = {}
            if self.settings.multilinesearch:
                self.search_text_file_contents(fo, f)
            else:
                self.search_text_file_lines(fo, f)
            fo.close()
        except IOError as e:
            print 'IOError: {0!s}: {1}'.format(e, f)

    def get_line_count(self, s):
        return len(re.findall(r'(\r\n|\n)', s))

    def search_text_file_contents(self, fo, filename=''):
        """Search in a given text file object contents (all at once)
           and return the results
        """
        results  = {}
        contents = fo.read()
        for s in self.settings.searchpatterns:
            if s in results and self.settings.firstmatch:
                continue
            matches = s.finditer(contents)
            for m in matches:
                before_line_count = self.get_line_count(m.string[0:m.start()])
                after_line_count = self.get_line_count(m.string[m.end():])
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

    def lines_match(self, lines, in_patterns, out_patterns):
        if (not in_patterns or \
            self.any_matches_any_pattern(lines, in_patterns)) and \
           (not out_patterns or \
            not self.any_matches_any_pattern(lines, out_patterns)):
            return True
        return False

    def lines_before_match(self, lines_before):
        return self.lines_match(lines_before,
            self.settings.in_linesbeforepatterns,
            self.settings.out_linesbeforepatterns)

    def lines_after_match(self, lines_after):
        return self.lines_match(lines_after,
            self.settings.in_linesafterpatterns,
            self.settings.out_linesafterpatterns)

    def search_text_file_lines(self, fo, filename=''):
        """Search in a given text file object by line and return the results"""
        results  = {}
        linenum = 0
        lines_before = deque()
        lines_after = deque()
        while True:
            if lines_after:
                line = lines_after.popleft()
            else:
                try:
                    line = fo.next()
                except StopIteration:
                    break
                except AttributeError as e:
                    #print 'AttributeError: %s' % e
                    break
            linenum += 1
            if self.settings.numlinesafter:
                while len(lines_after) < self.settings.numlinesafter:
                    try:
                        lines_after.append(fo.next())
                    except StopIteration:
                        break
            for s in self.settings.searchpatterns:
                if s in results and self.settings.firstmatch:
                    continue
                if s.search(line):
                    if (lines_before and \
                        not self.lines_before_match(lines_before)) or \
                        (lines_after and \
                            not self.lines_after_match(lines_after)):
                        continue
                    search_result = SearchResult(pattern=s.pattern,
                                                 filename=filename,
                                                 linenum=linenum, line=line,
                                                 lines_before=list(lines_before),
                                                 lines_after=list(lines_after))
                    self.add_search_result(search_result)
                    if s in results:
                        results[s].append(search_result)
                    else:
                        results[s] = [search_result]
            if self.settings.numlinesbefore:
                if len(lines_before) == self.settings.numlinesbefore:
                    lines_before.popleft()
                if len(lines_before) < self.settings.numlinesbefore:
                    lines_before.append(line)

    def search_compressed_file(self, f):
        """Search a compressed file, return number of matches found"""
        matchesfound = 0
        ext = File.get_extension(f)
        if not ext: return
        ext = ext.lower()
        if ext in ('zip', 'jar', 'war', 'ear'):
            # handle zip files
            if self.settings.debug:
                print 'searching {0} file: {1}'.format(ext, f) 
            try:
                matchesfound = self.search_zip_file(f)
            except zipfile.BadZipfile as e:
                if not ext == 'ear':
                    print 'BadZipfile: {0!s}: {1}'.format(e, f)
        elif ext in ('bz2', 'tar', 'tgz', 'gz') and tarfile.is_tarfile(f):
            # handle tar files
            if self.settings.debug:
                print 'searching {0} file: {1}'.format(ext, f) 
            try:
                matchesfound = self.search_tar_file(f, ext)
            except Exception as e:
                msg = 'Exception while searching a tar file {0}: {1!s}'
                print msg.format(f, e)

    def search_zip_file(self, f):
        """Search a jar/zip file, return number of matches found"""
        matchesfound = 0
        z = zipfile.ZipFile(f, 'r')
        zipinfos = z.infolist()
        for zipinfo in zipinfos:
            if self.settings.debug:
                print 'zipinfo.filename: {0}'.format(zipinfo.filename)
            if zipinfo.file_size and not self.filter_file(zipinfo.filename):
                if self.settings.debug:
                    msg = 'file_size and not filter_file: {0}'
                    print msg.format(zipinfo.filename)
                if self.is_text_file(zipinfo.filename):
                    if self.settings.debug:
                        msg = 'searchable text file in zip: {0}'
                        print msg.format(zipinfo.filename)
                    sio = StringIO(z.read(zipinfo.filename))
                    sio.seek(0)
                    results = \
                        self.search_text_file_obj(sio, f, zipinfo.filename)
                    matchesfound = len(results)
                    for pattern in results:
                        for search_result in results[pattern]:
                            #print '%s:%d:%s' % (f, linenum, line)
                            self.add_search_result(search_result)
                elif self.is_searchable_file(zipinfo.filename):
                    if self.settings.debug:
                        msg = 'searchable binary file in zip: {0}'
                        print msg.format(zipinfo.filename)

    def search_tar_file(self, f, ext):
        """Search a tar file, return number of matches found"""
        matchesfound = 0
        try:
            tar = tarfile.open(f, 'r:'+ext)
            for tarinfo in tar:
                if tarinfo.isreg() and not self.filter_file(tarinfo.name):
                    if self.settings.debug:
                        msg = 'isreg and not filter_file: {0}'
                        print msg.format(tarinfo.name)
                    if self.is_text_file(tarinfo.name):
                        if self.settings.debug:
                            msg = 'searchable text file in tar: {0}'
                            print msg.format(tarinfo.name)
                        tf = tar.extractfile(tarinfo)
                        results = self.search_text_file_obj(tf, f, tarinfo.name)
                        matchesfound = len(results)
                        for pattern in results:
                            for search_result in results[pattern]:
                                self.add_search_result(search_result)
                    elif  self.is_searchable_file(tarinfo.name):
                        if self.settings.debug:
                            msg = 'searchable binary file in tar: {0}'
                            print msg.format(tarinfo.name)
            tar.close()
        except tarfile.CompressionError as e:
            if not ext == 'tgz':
                msg = 'CompressionError while trying to open {0}: {1!s}'
                print msg.format(f, e)

    def add_search_result(self, search_result):
        """Add to list of search results"""
        self.results.append(search_result)
        pattern = search_result.pattern
        fullfile = os.path.abspath(search_result.filename)
        self.rescounts[pattern] = self.rescounts.setdefault(pattern, 0) + 1
        self.patterndict.setdefault(pattern, list()).append(search_result)
        self.filedict.setdefault(fullfile, list()).append(search_result)
        if self.settings.printresults:
            self.print_result(search_result)

    def print_result(self, search_result):
        """Print the current search result info to the console"""
        sio = StringIO()
        if len(self.settings.searchpatterns) > 1:
            sio.write('{0}: '.format(search_result.pattern))
        sio.write(str(search_result))
        s = sio.getvalue()
        sio.close()
        try:
            print s
        except UnicodeEncodeError as e:
            print repr(s)

    def print_res_counts(self):
        """Print result counts"""
        for p in self.patterndict:
            if p in self.rescounts:
                match = 'match'
                if self.rescounts[p] > 1:
                    match += 'es'
                print '{0} {1} for "{2}"'.format(self.rescounts[p], match, p)
            else:
                print '0 matches for "{0}"'.format(p)

    def get_matching_files(self, pattern=None):
        """Get list of files with matches for a given pattern
           (or all patterns if none given)"""
        patterns = []
        if pattern:
            patterns.append(pattern)
        else:
            patterns.extend(self.patterndict.keys())
        file_list = set()
        for p in patterns:
            file_list.update([
                os.path.abspath(r.filename) for r in self.patterndict[p]])
        file_list = list(file_list)
        file_list.sort()
        return file_list

    def get_matching_lines(self, pattern=None):
        """Get list of lines with matches for a given pattern (or all patterns
           if none given)"""
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
