# -*- coding: utf-8 -*-
################################################################################
#
# searcher.py
#
# class Searcher: executes a file search
#
################################################################################
from collections import deque
from cStringIO import StringIO
from datetime import datetime, timedelta
from pprint import pprint
import os
import re

TARFILE_MODULE_AVAILABLE = True
ZIPFILE_MODULE_AVAILABLE = True

try:
    import tarfile
except ImportError as e:
    print('tarfile not imported: {0!s}'.format(e))
    TARFILE_MODULE_AVAILABLE = False
try:
    import zipfile
except ImportError as e:
    print('zipfile not imported: {0!s}'.format(e))
    ZIPFILE_MODULE_AVAILABLE = False

from filetypes import FileType, FileTypes
from fileutil import FileUtil
from searchfile import SearchFile
from searchresult import SearchResult

class Searcher:
    """a class to search files"""

    def __init__(self, settings, **kargs):
        self.settings = settings
        self.validate_settings()
        self.filetypes = FileTypes()
        self.results = []
        self.patterndict = {}
        self.timers = {}
        self.total_elapsed = timedelta()
        self.filedict = {}
        self.rescounts = {}
        self.__dict__.update(kargs)

    def log(self, message):
        """log a message (for now just print to stdout)"""
        print(message)

    def validate_settings(self):
        """Assert required settings in SearchSettings instance"""
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
        return FileUtil.get_extension(f)

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
        if not self.filetypes.is_archive_file(f):
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
            self.log('get_search_dirs()')
        searchdirs = []
        if os.path.isdir(self.settings.startpath):
            if self.is_search_dir(os.path.abspath(self.settings.startpath)):
                searchdirs.append(self.settings.startpath)
            if self.settings.recursive:
                for root, dirs, files in os.walk(self.settings.startpath):
                    searchdirs.extend([
                        os.path.join(root, d) for d in dirs \
                        if self.is_search_dir(os.path.join(root, d))])
        elif os.path.isfile(self.settings.startpath):
            d = os.path.dirname(self.settings.startpath)
            if not d:
                d = '.'
            searchdirs.append(d)
        return searchdirs

    def get_search_files(self, searchdirs):
        """Get the list of files to search"""
        if self.settings.debug:
            self.log('get_search_files()')
        searchfiles = []
        if os.path.isdir(self.settings.startpath):
            for d in searchdirs:
                searchfiles.extend(self.get_search_files_for_directory(d))
        elif os.path.isfile(self.settings.startpath):
            (d,f) = os.path.split(self.settings.startpath)
            if not d:
                d = '.'
            searchfiles.append(SearchFile(path=d,
                                          filename=f,
                                          filetype=self.filetypes.get_filetype(f)))
        return searchfiles

    def get_search_files_for_directory(self, d):
        """Get the list of files to search in a given directory"""
        files = [f for f in os.listdir(d) if os.path.isfile(os.path.join(d, f))]
        searchfiles = [SearchFile(path=d,
                        filename=f,
                        filetype=self.filetypes.get_filetype(f))
                        for f in files]
        return [sf for sf in searchfiles
                if (self.settings.searcharchives and 
                    sf.filetype == FileType.Archive and
                    self.is_archive_search_file(sf.filename))
                or (not self.settings.archivesonly and
                    self.is_search_file(sf.filename))]

    def add_timer(self, name, action):
        self.timers[name+':'+action] = datetime.now()

    def start_timer(self, name):
        self.add_timer(name, 'start')

    def stop_timer(self, name):
        self.add_timer(name, 'stop')
        self.add_elapsed(name)

    def add_elapsed(self, name):
        self.total_elapsed += self.get_elapsed(name)

    def get_elapsed(self, name):
        start = self.timers[name+':start']
        stop = self.timers[name+':stop']
        elapsed = stop - start
        return elapsed

    def print_elapsed(self, name):
        ms = self.get_elapsed(name).total_seconds() * 1000
        self.log('Elapsed time for {0}: {1} ms'.format(name, ms))

    def print_total_elapsed(self):
        msg = 'Total elapsed time: {0} ms'
        ms = self.total_elapsed.total_seconds() * 1000
        self.log(msg.format(ms))

    def search(self):
        """Search files to find instances of searchpattern(s) starting from
           startpath"""
        # get the searchdirs
        if self.settings.dotiming:
            self.start_timer('get_search_dirs')
        searchdirs = self.get_search_dirs()
        if self.settings.dotiming:
            self.stop_timer('get_search_dirs')
            if self.settings.printresults:
                self.print_elapsed('get_search_dirs')
        if self.settings.verbose:
            self.log('\nDirectories to be searched ({0}):'.format(len(searchdirs)))
            for d in searchdirs:
                self.log(d)
            self.log("")

        # get the searchfiles
        if self.settings.dotiming:
            self.start_timer('get_search_files')
        searchfiles = self.get_search_files(searchdirs)
        if self.settings.dotiming:
            self.stop_timer('get_search_files')
            if self.settings.printresults:
                self.print_elapsed('get_search_files')
        if self.settings.verbose:
            self.log('\nFiles to be searched ({0}):'.format(len(searchfiles)))
            for f in searchfiles:
                self.log(f)
            self.log("")
        if self.settings.dotiming:
            self.start_timer('search_files')
        for sf in searchfiles:
            self.search_file(sf)
        if self.settings.dotiming:
            self.stop_timer('search_files')
            if self.settings.printresults:
                self.print_elapsed('search_files')
                self.print_total_elapsed()

    def print_results(self):
        self.log('Search results (%d):' % len(self.results))
        for r in self.results:
           self.print_result(r)
        self.print_res_counts()

    def search_file(self, sf):
        """Search in a file, return number of matches found"""
        if not self.filetypes.is_searchable_file(sf.filename):
            if self.settings.verbose or self.settings.debug:
                self.log('Skipping unsearchable file: {0}'.format(f))
            return
        if sf.filetype == FileType.Text:
            self.search_text_file(sf)
        elif sf.filetype == FileType.Binary:
            self.search_binary_file(sf)
        elif sf.filetype == FileType.Archive and self.settings.searcharchives:
            try:
                self.search_archive_file(sf)
            except IOError as e:
                self.log('IOError: {0!s}: {1!s}'.format(e, sf))

    def search_binary_file(self, sf):
        """Search a binary file"""
        if self.settings.debug:
            self.log("Searching binary file {0!s}".format(sf.relativepath))
        try:
            fo = open(sf.relativepath, 'rb')
            self.search_binary_file_obj(sf, fo)
            fo.close()
        except IOError as e:
            self.log('IOError: {0!s}: {1!s}'.format(e, sf))

    def search_binary_file_obj(self, sf, fo):
        """Search a binary file file object"""
        contents = fo.read()
        for s in self.settings.searchpatterns:
            if s.search(contents):
                search_result = SearchResult(pattern=s.pattern,
                                             # TODO: switch to SearchFile instance
                                             filename=str(sf),
                                             linenum=0,
                                             line=None)
                self.add_search_result(search_result)

    def search_text_file(self, sf, enc=None):
        """Search a text file"""
        if self.settings.debug:
            self.log("Searching text file {0!s}".format(sf.relativepath))
        try:
            fo = open(sf.relativepath, 'r')
            self.search_text_file_obj(sf, fo)
            fo.close()
        except IOError as e:
            self.log('IOError: {0!s}: {1}'.format(e, sf))

    def search_text_file_obj(self, sf, fo):
        """Search a text file file object"""
        if self.settings.multilinesearch:
            self.search_text_file_contents(sf, fo)
        else:
            self.search_text_file_lines(sf, fo)

    def search_text_file_contents(self, sf, fo):
        """Search a given text file object contents all at once
        """
        contents = fo.read()
        search_results = self.search_multiline_string(contents)
        for search_result in search_results:
            search_result.filename = str(sf)
            self.add_search_result(search_result)

    def get_new_line_indices(self, s):
        indices = []
        for i, c in enumerate(s):
            if c == '\n':
                indices.append(i)
        return indices

    def search_multiline_string(self, s):
        """Search a given searchable string possibly containing multiple newlines
           and return a list of SearchResult instances (without filename)
        """
        search_results = []
        new_line_indices = self.get_new_line_indices(s)
        start_line_indices = [0] + [i+1 for i in new_line_indices]
        end_line_indices = new_line_indices + [len(s) - 1]
        for p in self.settings.searchpatterns:
            search_results.extend(
                self.search_multiline_string_for_pattern(s, p, start_line_indices,
                    end_line_indices))
        return search_results

    def get_lines_before(self, s, start_line_index, before_start_indices):
        if not before_start_indices:
            return []
        line_count = self.settings.numlinesbefore
        if len(before_start_indices) >= line_count:
            before_start_indices = before_start_indices[(line_count*-1):]
        lines_before = []
        for i, x in enumerate(before_start_indices[:-1]):
            if i < len(before_start_indices) - 1:
                lines_before.append(
                    s[before_start_indices[i]:before_start_indices[i+1]-1])
        lines_before.append(s[before_start_indices[-1]:start_line_index-1])
        return lines_before

    def get_lines_after_to_or_until(self, s, after_start_indices):
        lines_after_patterns = []
        if self.settings.linesaftertopatterns:
            lines_after_patterns = self.settings.linesaftertopatterns
        else:
            lines_after_patterns = self.settings.linesafteruntilpatterns
        lines_after = []
        match_indices = {}
        for p in lines_after_patterns:
            m = p.search(s[after_start_indices[0]:])
            if m:
                match_indices[p] = m.start() + after_start_indices[0]
        if match_indices:
            first_index = min(x for x in match_indices.values())
            for i, x in enumerate(after_start_indices):
                if x < first_index and i < len(after_start_indices) - 1:
                    lines_after.append(
                        s[after_start_indices[i]:after_start_indices[i+1]-1])
        if self.settings.linesaftertopatterns:
            return lines_after
        else:
            return lines_after[:-1]

    def get_lines_after(self, s, after_start_indices):
        if not after_start_indices:
            return []
        if self.do_lines_after_or_until():
            return self.get_lines_after_to_or_until(s, after_start_indices)
        line_count = self.settings.numlinesafter
        if len(after_start_indices) > line_count:
            after_start_indices = after_start_indices[0:line_count+1]
        elif len(after_start_indices) == line_count:
            after_start_indices.append(len(s)-1)
        lines_after = []
        for i, x in enumerate(after_start_indices):
            if i < len(after_start_indices) - 1:
                lines_after.append(
                    s[after_start_indices[i]:after_start_indices[i+1]-1])
        return lines_after

    def do_lines_after_or_until(self):
        if self.settings.linesaftertopatterns or \
            self.settings.linesafteruntilpatterns:
            return True
        return False

    def do_lines_after(self):
        if self.settings.numlinesafter or self.do_lines_after_or_until():
            return True
        return False

    def search_multiline_string_for_pattern(self, s, p, start_line_indices,
        end_line_indices):
        """Search a given searchable string possibly containing multiple newlines
           and a specific pattern and return a list of SearchResult instances
           (without filename)
        """
        lines_before = []
        lines_after = []
        search_results = []
        matches = p.finditer(s)
        for m in matches:
            m_line_start_index = 0
            m_line_end_index = len(s) - 1
            before_start_indices = [x for x in start_line_indices if x < m.start()]
            before_line_count = 0
            if before_start_indices:
                m_line_start_index = before_start_indices[-1]
                before_line_count = len(before_start_indices) - 1
            m_line_end_index = end_line_indices[start_line_indices.index(m_line_start_index)]
            after_start_indices = [x for x in start_line_indices if x > m.start()]
            line = s[m_line_start_index:m_line_end_index]
            if self.settings.numlinesbefore and before_line_count:
                lines_before = self.get_lines_before(s, m_line_start_index,
                    before_start_indices[:-1])
            if self.do_lines_after():
                lines_after = self.get_lines_after(s, after_start_indices)
                if after_start_indices and not lines_after:
                    continue
            if (lines_before and
                not self.lines_before_match(lines_before)) or \
                (lines_after and
                 not self.lines_after_match(lines_after)):
                continue
            match_start_index = m.start() - m_line_start_index + 1
            match_end_index = m.end() - m_line_start_index + 1
            search_result = SearchResult(pattern=p.pattern,
                                         linenum=before_line_count + 1,
                                         line=line,
                                         match_start_index=match_start_index,
                                         match_end_index=match_end_index,
                                         lines_before=list(lines_before),
                                         lines_after=list(lines_after))
            search_results.append(search_result)
            if self.settings.firstmatch:
                break
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
        if self.do_lines_after_or_until():
            return True
        return self.lines_match(lines_after,
            self.settings.in_linesafterpatterns,
            self.settings.out_linesafterpatterns)

    def search_text_file_lines(self, sf, fo):
        """Search in a given text file object by line and return the results"""
        search_results = self.search_line_iterator(fo)
        for search_result in search_results:
            search_result.filename = str(sf)
            self.add_search_result(search_result)

    def search_line_iterator(self, lines):
        """Consecutively search the lines of a line iterator and return results"""
        pattern_match_dict = {}
        linenum = 0
        lines_before = deque()
        lines_after = deque()
        results = []
        while True:
            if lines_after:
                line = lines_after.popleft()
            else:
                try:
                    line = lines.next().rstrip('\r\n')
                except StopIteration:
                    break
                except AttributeError as e:
                    #self.log('AttributeError: %s' % e)
                    break
            linenum += 1
            if self.settings.numlinesafter:
                while len(lines_after) < self.settings.numlinesafter:
                    try:
                        lines_after.append(lines.next().rstrip('\r\n'))
                    except StopIteration:
                        break
            for p in self.settings.searchpatterns:
                if self.settings.firstmatch and p in pattern_match_dict:
                    continue
                # find all matches for the line
                matchiter = p.finditer(line)
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
                        if self.do_lines_after_or_until():
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
                            while not lines_after_to_match and \
                                  not lines_after_until_match:
                                try:
                                    next_line = lines.next().rstrip('\r\n')
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
                        sr_lines_after = []
                        if self.do_lines_after_or_until():
                            if lines_after_to_match:
                                sr_lines_after = list(lines_after)
                            elif lines_after_until_match:
                                sr_lines_after = list(lines_after)[:-1]
                            else:
                                continue
                        search_result = \
                            SearchResult(pattern=p.pattern,
                                         linenum=linenum,
                                         line=line,
                                         match_start_index=match.start() + 1,
                                         match_end_index=match.end() + 1,
                                         lines_before=list(lines_before),
                                         lines_after=sr_lines_after,
                                         maxlinelength=self.settings.maxlinelength)
                        results.append(search_result)
                        pattern_match_dict[p] = 1
            if self.settings.numlinesbefore:
                if len(lines_before) == self.settings.numlinesbefore:
                    lines_before.popleft()
                if len(lines_before) < self.settings.numlinesbefore:
                    lines_before.append(line)
        return results

    def search_archive_file(self, sf):
        """Search an archive (compressed) file"""
        ext = FileUtil.get_extension(sf.filename)
        if not ext: return
        if self.settings.debug:
            self.log('Searching {0} file {1}'.format(ext, sf))
        if ext in ('zip', 'jar', 'war', 'ear'):
            # handle zip files
            try:
                self.search_zip_file(sf)
            except zipfile.BadZipfile as e:
                if not ext == 'ear':
                    self.log('BadZipfile: {0!s}: {1}'.format(e, sf))
        elif ext in ('bz2', 'tar', 'tgz', 'gz') and \
             tarfile.is_tarfile(sf.relativepath):
            # handle tar files
            try:
                self.search_tar_file(sf, ext)
            except Exception as e:
                msg = 'Exception while searching a tar file {0}: {1!s}'
                self.log(msg.format(sf, e))
        else:
            msg = 'Searching archive file type "{0}" is unsupported at this time'
            self.log(msg.format(ext))

    def search_zip_file(self, sf):
        """Search an archive file compressed with zip"""
        zfo = zipfile.ZipFile(sf.relativepath, 'r')
        self.search_zipfile_obj(sf, zfo)

    def search_zipfile_obj(self, sf, zfo):
        """Search a ZipFile object"""
        zipinfos = zfo.infolist()
        for zipinfo in zipinfos:
            #if self.settings.debug:
            #   self.log('zipinfo.filename: {0}'.format(zipinfo.filename))
            if zipinfo.file_size and self.is_search_file(zipinfo.filename):
                zio = StringIO(zfo.read(zipinfo.filename))
                zio.seek(0)
                nsf = SearchFile(containers=sf.containers + [sf.relativepath])
                nsf.path, nsf.filename = os.path.split(zipinfo.filename)
                self.search_zipinfo_obj(nsf, zio)

    def search_zipinfo_obj(self, sf, zio):
        """Search a ZipInfo object"""
        if self.filetypes.is_text_file(sf.filename):
            #if self.settings.debug:
            #   msg = 'searchable text file in zip: {0}'
            #   self.log(msg.format(zio.filename))
            self.search_text_file_obj(sf, zio)
        elif self.filetypes.is_binary_file(sf.filename):
            #if self.settings.debug:
            #   msg = 'searchable binary file in zip: {0}'
            #   self.log(msg.format(zipinfo.filename))
            self.search_binary_file_obj(sf, zio)

    def search_tar_file(self, sf, ext):
        """Search a tar file"""
        try:
            tfo = tarfile.open(sf.relativepath, 'r:'+ext)
            self.search_tarfile_obj(sf, tfo)
            tfo.close()
        except tarfile.CompressionError as e:
            if not ext == 'tgz':
                msg = 'CompressionError while trying to open {0}: {1!s}'
                self.log(msg.format(sf, e))

    def search_tarfile_obj(self, sf, tar):
        """Search a tarfile object"""
        for tarinfo in tar:
            #if tarinfo.isreg() and not self.filter_file(tarinfo.name):
            if tarinfo.isfile() and self.is_search_file(tarinfo.name):
                #if self.settings.debug:
                #    msg = 'isfile and is_search_file: {0}'
                #    self.log(msg.format(tarinfo.name))
                #tio = tar.extractfile(tarinfo)
                tio = StringIO(tar.extractfile(tarinfo).read())
                tio.seek(0)
                nsf = SearchFile(containers=sf.containers + [sf.relativepath])
                nsf.path, nsf.filename = os.path.split(tarinfo.name)
                #self.log('nsf.path: {0}'.format(nsf.path))
                #self.log('nsf.filename: {0}'.format(nsf.filename))
                self.search_tarinfo_obj(nsf, tio)

    def search_tarinfo_obj(self, sf, tio):
        """Search a tarinfo object"""
        if self.filetypes.is_text_file(sf.filename):
            if self.settings.debug:
                self.log('searchable text file in tar: {0}'.format(sf.filename))
            self.search_text_file_obj(sf, tio)
        elif  self.filetypes.is_binary_file(sf.filename):
            if self.settings.debug:
                self.log('searchable binary file in tar: {0}'.format(sf.filename))
            self.search_binary_file_obj(sf, tio)

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
            self.log(s)
        except UnicodeEncodeError as e:
            self.log(repr(s))

    def print_res_counts(self):
        """Print result counts"""
        for p in self.patterndict:
            if p in self.rescounts:
                match = 'match'
                if self.rescounts[p] > 1:
                    match += 'es'
                self.log('{0} {1} for "{2}"'.format(self.rescounts[p], match, p))
            else:
                self.log('0 matches for "{0}"'.format(p))

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

