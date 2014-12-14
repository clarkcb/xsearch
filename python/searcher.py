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

    def validate_settings(self):
        assert self.settings.startpath, 'Startpath not defined'
        assert os.path.exists(self.settings.startpath), 'Startpath not found'
        assert self.settings.searchpatterns, 'No search patterns specified'

    def matches_any_pattern(self, s, pattern_set):
        """Returns true if string s matches any pattern in pattern_set, else
           false"""
        return any(p.search(s) for p in pattern_set)

    def any_matches_any_pattern(self, slist, pattern_set):
        """Returns true if any string in slist matches any pattern in
        pattern_set, else false"""
        for s in slist:
            if self.matches_any_pattern(s, pattern_set):
                return True
        return False

    def get_ext(self, f):
        return self.fileutil.get_extension(f)

    def is_search_dir(self, d):
        path_elems = [p for p in d.split(os.sep) if p not in ('.', '..')]
        if self.settings.excludehidden:
            for p in path_elems:
                if p.startswith('.'):
                    return False
        if self.settings.in_dirpatterns and \
            not self.any_matches_any_pattern(path_elems, self.settings.in_dirpatterns):
            return False
        if self.settings.out_dirpatterns and \
            self.any_matches_any_pattern(path_elems, self.settings.out_dirpatterns):
            return False
        return True

    def is_archive_search_file(self, f):
        if not self.fileutil.is_archive_file(f):
            return False
        if f.startswith('.') and self.settings.excludehidden:
            return False
        if self.settings.in_archiveextensions and \
            not self.get_ext(f) in self.settings.in_archiveextensions:
            return False
        if self.settings.out_archiveextensions and \
            self.get_ext(f) in self.settings.out_archiveextensions:
            return False
        if self.settings.in_archivefilepatterns and \
            not self.matches_any_pattern(f, self.settings.in_archivefilepatterns):
            return False
        if self.settings.out_archivefilepatterns and \
            self.matches_any_pattern(f, self.settings.out_archivefilepatterns):
            return False
        return True

    def is_search_file(self, f):
        if f.startswith('.') and self.settings.excludehidden:
            return False
        if self.settings.in_extensions and \
            not self.get_ext(f) in self.settings.in_extensions:
            return False
        if self.settings.out_extensions and \
            self.get_ext(f) in self.settings.out_extensions:
            return False
        if self.settings.in_filepatterns and \
            not self.matches_any_pattern(f, self.settings.in_filepatterns):
            return False
        if self.settings.out_filepatterns and \
            self.matches_any_pattern(f, self.settings.out_filepatterns):
            return False
        return True

    def get_search_dirs(self):
        """Get the list of directories to search"""
        if self.settings.debug:
            print 'get_search_dirs()'
        searchdirs = []
        if self.is_search_dir(os.path.abspath(self.settings.startpath)):
            searchdirs.append(self.settings.startpath)
        if self.settings.recursive:
            for root, dirs, files in os.walk(self.settings.startpath):
                searchdirs.extend([
                    os.path.join(root,d) for d in dirs \
                    if self.is_search_dir(os.path.join(root,d))])
        return searchdirs

    def get_search_files(self, searchdirs):
        """Get the list of files to search"""
        if self.settings.debug:
            print 'get_search_files()'
        searchfiles = []
        for d in searchdirs:
            searchfiles.extend(self.get_search_files_for_directory(d))
        return searchfiles

    def get_search_files_for_directory(self, d):
        """Get the list of files to search in a given directory"""
        files = [f for f in os.listdir(d) if os.path.isfile(os.path.join(d,f))]
        return [os.path.join(d,f) for f in files
                if (self.settings.searcharchives and self.is_archive_search_file(f))
                or (not self.settings.archivesonly and self.is_search_file(f))]

    def add_timer(self, name, action):
        self.timers[name+':'+action] = datetime.now()

    def start_timer(self, name):
        self.add_timer(name, 'start')

    def stop_timer(self, name):
        self.add_timer(name, 'stop')
        if self.settings.printresults:
            self.print_elapsed(name)

    def get_elapsed(self, name):
        start = self.timers[name+':start']
        stop = self.timers[name+':stop']
        return stop - start

    def print_elapsed(self, name):
        elapsed = self.get_elapsed(name)
        print 'Elapsed time for {0}: {1}'.format(name, elapsed)

    def search(self):
        """Search files to find instances of searchpattern(s) starting from
           startpath"""
        # get the searchdirs
        if self.settings.dotiming:
            self.start_timer('get_search_dirs')
        searchdirs = self.get_search_dirs()
        if self.settings.dotiming:
            self.stop_timer('get_search_dirs')
        if self.settings.verbose:
            print '\nDirectories to be searched ({0}):'.format(len(searchdirs))
            for d in searchdirs:
                print d
            print

        # get the searchfiles
        if self.settings.dotiming:
            self.start_timer('get_search_files')
        searchfiles = self.get_search_files(searchdirs)
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

    def print_results(self):
        print 'Search results (%d):' % len(self.results)
        for r in self.results:
           self.print_result(r)
        self.print_res_counts()

    def search_file(self, f):
        """Search in a file, return number of matches found"""
        if not self.fileutil.is_searchable_file(f):
            if self.settings.verbose or self.settings.debug:
                print 'Skipping unsearchable file: {0}'.format(f)
            return
        if self.fileutil.is_text_file(f):
            self.search_text_file(f)
        elif self.fileutil.is_binary_file(f):
            self.search_binary_file(f)
        elif self.fileutil.is_archive_file(f) and self.settings.searcharchives:
            try:
                self.search_archive_file(f)
            except IOError as e:
                print 'IOError: {0!s}: {1}'.format(e, f)

    def search_binary_file(self, f):
        """Search a binary file"""
        try:
            fo = open(f, 'rb')
            self.search_binary_file_obj(fo, f)
            fo.close()
        except IOError as e:
            print 'IOError: {0!s}: {1}'.format(e, f)

    def search_binary_file_obj(self, fo, f):
        """Search a binary file file object"""
        contents = fo.read()
        for s in self.settings.searchpatterns:
            if s.search(contents):
                search_result = SearchResult(pattern=s.pattern,
                                             filename=f,
                                             linenum=0,
                                             line=None)
                self.add_search_result(search_result)

    def search_text_file(self, f, enc=None):
        """Search a text file"""
        try:
            fo = open(f, 'r')
            self.search_text_file_obj(fo, f)
            fo.close()
        except IOError as e:
            print 'IOError: {0!s}: {1}'.format(e, f)

    def search_text_file_obj(self, fo, f):
        """Search a text file file object"""
        if self.settings.multilinesearch:
            self.search_text_file_contents(fo, f)
        else:
            self.search_text_file_lines(fo, f)

    def get_line_count(self, s):
        return len(re.findall(r'(\r\n|\n)', s))

    def line_indices_for_current_index(self, index, contents):
        """Get the start and end indices into contents that represent the
           beginning and end of the line containing the character at index
        """
        startindex = endindex = index
        while startindex > 0 and contents[startindex] != '\n':
            startindex -= 1
        if contents[startindex] == '\n':
            startindex += 1
        while endindex < len(contents) and contents[endindex] != '\n':
            endindex += 1
        if endindex < len(contents) and contents[endindex] == '\n':
            endindex += 1
        if startindex == endindex:
            startindex -= 1
        return startindex, endindex

    def search_text_file_contents(self, fo, filename=''):
        """Search a given text file object contents all at once
        """
        contents = fo.read()
        search_results = self.search_multiline_string(contents)
        for search_result in search_results:
            search_result.filename = filename
            self.add_search_result(search_result)

    def search_multiline_string(self, s):
        """Search a given searchable string possibly containing multiple newlines
           and return a list of SearchResult instances (without filename)
        """
        search_results = []
        for p in self.settings.searchpatterns:
            search_results.extend(self.search_multiline_string_for_pattern(s, p))
        return search_results

    def search_multiline_string_for_pattern(self, s, p):
        """Search a given searchable string possibly containing multiple newlines
           and a specific pattern and return a list of SearchResult instances
           (without filename)
        """
        lines_before = deque()
        lines_after = deque()
        search_results = []
        matches = s.finditer(contents)
        for m in matches:
            m_line_start_index, m_line_end_index = \
                self.line_indices_for_current_index(m.start(), contents)
            before_line_count = 0
            if m_line_start_index > 0:
                before_line_count = self.get_line_count(contents[0:m.start()])
            line = contents[m_line_start_index:m_line_end_index]
            if self.settings.debug:
                print 'line: "{0}"'.format(line)
            if self.settings.numlinesbefore and before_line_count:
                b_line_start_index, b_line_end_index = m_line_start_index, m_line_end_index
                while b_line_start_index > 0 and \
                      len(lines_before) < self.settings.numlinesbefore:
                    b_line_start_index, b_line_end_index = \
                        self.line_indices_for_current_index(b_line_start_index-2, contents)
                    lines_before.appendleft(contents[b_line_start_index:b_line_end_index])
            if self.settings.debug:
                print 'lines_before: {0!s}'.format(lines_before)
            if self.settings.numlinesafter:
                a_line_start_index, a_line_end_index = m_line_end_index, m_line_end_index
                while a_line_end_index < len(contents) and \
                      len(lines_after) < self.settings.numlinesafter:
                    a_line_start_index, a_line_end_index = \
                        self.line_indices_for_current_index(a_line_end_index, contents)
                    lines_after.append(contents[a_line_start_index:a_line_end_index])
            if self.settings.debug:
                print 'lines_after: {0!s}'.format(lines_after)
            if self.settings.firstmatch:
                continue
            else:
                search_result = SearchResult(pattern=s.pattern,
                                             linenum=before_line_count+1,
                                             line=line,
                                             lines_before=list(lines_before),
                                             lines_after=list(lines_after))
                search_results.append(search_result)
        return search_results

    def lines_match(self, lines, in_patterns, out_patterns):
        if (not in_patterns or
            self.any_matches_any_pattern(lines, in_patterns)) and \
           (not out_patterns or
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
        file_pattern_matches = {}
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
                # find all matches for the line
                matchiter = s.finditer(line)
                while True:
                    try:
                        match = matchiter.next()
                    except StopIteration:
                        break
                    else:
                        # if there are lines_before or lines_after and none matches
                        # then continue
                        if (lines_before and
                            not self.lines_before_match(lines_before)) or \
                            (lines_after and
                             not self.lines_after_match(lines_after)):
                            continue
                        # capture lines after until a linesaftertopattern or
                        # linesafteruntilpattern is matched, if any are defined
                        lines_after_to_match = False
                        lines_after_until_match = False
                        if self.settings.linesaftertopatterns or \
                           self.settings.linesafteruntilpatterns:
                            # check to see if lines_after has a match
                            if self.settings.linesaftertopatterns and \
                               self.any_matches_any_pattern(lines_after,
                               self.settings.linesaftertopatterns):
                               lines_after_to_match = True
                            if self.settings.linesafteruntilpatterns and \
                               self.any_matches_any_pattern(lines_after,
                               self.settings.linesafteruntilpatterns):
                               lines_after_until_match = True
                            # if not read in more lines until a match or EOF
                            while not lines_after_to_match and not lines_after_until_match:
                                try:
                                    next_line = fo.next()
                                    lines_after.append(next_line)
                                    if self.settings.linesaftertopatterns and \
                                       self.matches_any_pattern(next_line,
                                       self.settings.linesaftertopatterns):
                                        lines_after_to_match = True
                                    elif self.settings.linesafteruntilpatterns and \
                                         self.matches_any_pattern(next_line,
                                         self.settings.linesafteruntilpatterns):
                                        lines_after_until_match = True
                                except StopIteration:
                                    break
                        sr_lines_after = list(lines_after)
                        if lines_after_until_match:
                            sr_lines_after = sr_lines_after[:-1]

                        if self.settings.firstmatch and s in file_pattern_matches:
                            continue
                        else:
                            search_result = SearchResult(pattern=s.pattern,
                                                         filename=filename,
                                                         linenum=linenum,
                                                         line=line,
                                                         lines_before=list(lines_before),
                                                         lines_after=sr_lines_after,
                                                         maxlinelength=self.settings.maxlinelength,
                                                         match_start_index=match.start(),
                                                         match_end_index=match.end())
                            self.add_search_result(search_result)
                            file_pattern_matches[s] = 1
            if self.settings.numlinesbefore:
                if len(lines_before) == self.settings.numlinesbefore:
                    lines_before.popleft()
                if len(lines_before) < self.settings.numlinesbefore:
                    lines_before.append(line)

    def search_archive_file(self, f):
        """Search an archive (compressed) file"""
        ext = self.fileutil.get_extension(f)
        if not ext: return
        ext = ext.lower()
        if ext in ('zip', 'jar', 'war', 'ear'):
            # handle zip files
            if self.settings.debug:
                print 'searching {0} file: {1}'.format(ext, f) 
            try:
                self.search_zip_file(f)
            except zipfile.BadZipfile as e:
                if not ext == 'ear':
                    print 'BadZipfile: {0!s}: {1}'.format(e, f)
        elif ext in ('bz2', 'tar', 'tgz', 'gz') and tarfile.is_tarfile(f):
            # handle tar files
            if self.settings.debug:
                print 'searching {0} file: {1}'.format(ext, f) 
            try:
                self.search_tar_file(f, ext)
            except Exception as e:
                msg = 'Exception while searching a tar file {0}: {1!s}'
                print msg.format(f, e)

    def search_zip_file(self, f):
        """Search an archive file compressed with zip"""
        zf = zipfile.ZipFile(f, 'r')
        self.search_zipfile_obj(zf, f)

    def search_zipfile_obj(self, zf, f):
        """Search a ZipFile object"""
        zipinfos = zf.infolist()
        for zipinfo in zipinfos:
            if self.settings.debug:
                print 'zipinfo.filename: {0}'.format(zipinfo.filename)
            #if zipinfo.file_size and not self.filter_file(zipinfo.filename):
            if zipinfo.file_size and self.is_search_file(zipinfo.filename):
                if self.settings.debug:
                    msg = 'file_size and not filter_file: {0}'
                    print msg.format(zipinfo.filename)
                if self.fileutil.is_text_file(zipinfo.filename):
                    if self.settings.debug:
                        msg = 'searchable text file in zip: {0}'
                        print msg.format(zipinfo.filename)
                    sio = StringIO(zf.read(zipinfo.filename))
                    sio.seek(0)
                    self.search_text_file_obj(sio, f+'!'+zipinfo.filename)
                elif self.fileutil.is_binary_file(zipinfo.filename):
                    if self.settings.debug:
                        msg = 'searchable binary file in zip: {0}'
                        print msg.format(zipinfo.filename)
                    sio = StringIO(zf.read(zipinfo.filename))
                    sio.seek(0)
                    self.search_binary_file_obj(sio, f+'!'+zipinfo.filename)

    def search_tar_file(self, f, ext):
        """Search a tar file"""
        try:
            tf = tarfile.open(f, 'r:'+ext)
            self.search_tarfile_obj(tf, f)
            tf.close()
        except tarfile.CompressionError as e:
            if not ext == 'tgz':
                msg = 'CompressionError while trying to open {0}: {1!s}'
                print msg.format(f, e)

    def search_tarfile_obj(self, tar, f):
        """Search a tar file"""
        for tarinfo in tar:
            #if tarinfo.isreg() and not self.filter_file(tarinfo.name):
            if tarinfo.isreg() and self.is_search_file(tarinfo.name):
                if self.settings.debug:
                    msg = 'isreg and is_search_file: {0}'
                    print msg.format(tarinfo.name)
                if self.fileutil.is_text_file(tarinfo.name):
                    if self.settings.debug:
                        msg = 'searchable text file in tar: {0}'
                        print msg.format(tarinfo.name)
                    tf = tar.extractfile(tarinfo)
                    self.search_text_file_obj(tf, f+'!'+tarinfo.name)
                elif  self.fileutil.is_binary_file(tarinfo.name):
                    if self.settings.debug:
                        msg = 'searchable binary file in tar: {0}'
                        print msg.format(tarinfo.name)
                    tf = tar.extractfile(tarinfo)
                    self.search_binary_file_obj(tf, f+'!'+tarinfo.name)

    def add_search_result(self, search_result):
        """Add to list of search results"""
        self.results.append(search_result)
        pattern = search_result.pattern
        self.rescounts[pattern] = self.rescounts.setdefault(pattern, 0) + 1
        self.patterndict.setdefault(pattern, list()).append(search_result)
        fullfile = os.path.abspath(search_result.filename)
        self.filedict.setdefault(fullfile, list()).append(search_result)

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

    def get_matching_dirs(self, pattern=None):
        """Get list of dirs with matches for a given pattern
           (or all patterns if none given)"""
        patterns = []
        if pattern:
            patterns.append(pattern)
        else:
            patterns.extend(self.patterndict.keys())
        dir_list = set()
        for p in patterns:
            dir_list.update([
                os.path.dirname(r.filename) for r in self.patterndict[p]])
        dir_list = list(dir_list)
        dir_list.sort()
        return dir_list

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
        line_list = []
        for p in patterns:
            line_list.extend([r.line.strip() for r in self.patterndict[p]])
        if self.settings.uniquelines:
            line_list = list(set(line_list))
        line_list.sort()
        return line_list

