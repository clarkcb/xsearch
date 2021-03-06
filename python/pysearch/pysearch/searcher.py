# -*- coding: utf-8 -*-
"""
###############################################################################
#
# searcher.py
#
# class Searcher: executes a file search
#
###############################################################################
"""
import asyncio
import codecs
import os
from collections import deque
from io import StringIO
from typing import Deque, List, TextIO

from pyfind import FileType, FileUtil, Finder, FindFile

from .common import log
# from .filetypes import FileTypes
from .searchfile import SearchFile
from .searchresult import SearchResult
from .searchsettings import SearchSettings, PatternSet

TARFILE_MODULE_AVAILABLE = True
ZIPFILE_MODULE_AVAILABLE = True

try:
    import tarfile
except ImportError as ie:
    print('tarfile not imported: {0!s}'.format(ie))
    TARFILE_MODULE_AVAILABLE = False
try:
    import zipfile
except ImportError as ie:
    print('zipfile not imported: {0!s}'.format(ie))
    ZIPFILE_MODULE_AVAILABLE = False


class Searcher(object):
    """a class to search files"""

    __slots__ = ['settings', 'file_types', 'finder']

    def __init__(self, settings: SearchSettings):
        self.settings = settings
        self.finder = Finder(settings)
        self.__validate_settings()
        self.filetypes = self.finder.file_types

    def __validate_settings(self):
        """Assert required settings in SearchSettings instance"""
        assert self.settings.paths, 'Startpath not defined'
        for p in self.settings.paths:
            assert os.path.exists(p), 'Startpath not found'
            assert os.access(p, os.R_OK), 'Startpath not readable'
        assert self.settings.search_patterns, 'No search patterns defined'
        assert self.settings.lines_after >= 0, 'Invalid lines_after'
        assert self.settings.lines_before >= 0, 'Invalid lines_after'
        try:
            codecs.lookup(self.settings.text_file_encoding)
        except LookupError:
            raise AssertionError('Invalid encoding: {}'.format(
                self.settings.text_file_encoding))

    async def search(self) -> List[SearchResult]:
        """Search files to find instances of searchpattern(s) starting from
           startpath"""
        # get the searchfiles (now a single walkthrough)
        # searchfiles = self.get_search_files()
        findfiles = await self.finder.find()
        if self.settings.verbose:
            # searchdirs = sorted(list({sf.path for sf in searchfiles}))
            finddirs = sorted(list({sf.path for sf in findfiles}))
            log('\nDirectories to be searched ({0}):'.format(len(finddirs)))
            for d in finddirs:
                log(d)
            log('\n\nFiles to be searched ({0}):'.format(len(findfiles)))
            for f in findfiles:
                log(str(f))
            log("")

        # TODO: move to properties or settings
        batch_size = 250
        offset = 0
        search_results = []
        while offset < len(findfiles):
            to_index = min(offset + batch_size, len(findfiles))
            tasks = []
            for sf in findfiles[offset:to_index]:
                tasks.append(asyncio.create_task(self.search_file(sf)))
            for coroutine in asyncio.as_completed(tasks):
                search_results.extend(await coroutine)
            offset += batch_size
        return search_results

    async def search_file(self, sf: SearchFile) -> List[SearchResult]:
        """Search in a file, return number of matches found"""
        search_results = []
        if sf.file_type in self.file_types.TEXT_TYPES:
            search_results = self.search_text_file(sf)
        elif sf.file_type == FileType.BINARY:
            search_results = self.search_binary_file(sf)
        elif sf.file_type == FileType.ARCHIVE and self.settings.search_archives:
            try:
                search_results = self.search_archive_file(sf)
            except IOError as e:
                log('IOError: {0!s}: {1!s}'.format(e, sf))
        else:
            # it's UNKNOWN
            if self.settings.verbose:
                log('Skipping file with unknown type: {0}'.format(sf))
        return search_results

    def search_binary_file(self, sf: SearchFile) -> List[SearchResult]:
        """Search a binary file"""
        if self.settings.debug:
            log("Searching binary file {0!s}".format(sf.relative_path))
        search_results = []
        try:
            fo = open(sf.relative_path, mode='r', encoding='latin-1')
            search_results = self.__search_binary_file_obj(sf, fo)
            fo.close()
        except IOError as e:
            log('IOError: {0!s}: {1!s}'.format(e, sf))
        return search_results

    def __search_binary_file_obj(
            self, sf: SearchFile, fo: TextIO) -> List[SearchResult]:
        """Search a binary file object"""
        contents = fo.read()
        search_results = []
        for p in self.settings.search_patterns:
            matches = p.finditer(contents)
            for m in matches:
                search_result = SearchResult(pattern=p.pattern,
                                             file=sf,
                                             line_num=0,
                                             line='',
                                             match_start_index=m.start() + 1,
                                             match_end_index=m.end() + 1,
                                             lines_before=[],
                                             lines_after=[])
                search_results.append(search_result)
                if self.settings.first_match:
                    break
        return search_results

    def search_text_file(self, sf: SearchFile) -> List[SearchResult]:
        """Search a text file"""
        if self.settings.debug:
            log("Searching text file {0!s}".format(sf.relative_path))
        search_results = []
        try:
            fo = open(sf.relative_path, mode='r',
                      encoding=self.settings.text_file_encoding)
            search_results = self.__search_text_file_obj(sf, fo)
            fo.close()
        except IOError as e:
            log('IOError: {0!s}: {1}'.format(e, sf))
        return search_results

    def __search_text_file_obj(self, sf: SearchFile,
                               fo: TextIO) -> List[SearchResult]:
        """Search a text file object"""
        if self.settings.multi_line_search:
            return self.__search_text_file_contents(sf, fo)
        return self.__search_text_file_lines(sf, fo)

    def __search_text_file_contents(
            self, sf: SearchFile, fo: TextIO) -> List[SearchResult]:
        """Search a given text file object contents all at once
        """
        contents = fo.read()
        search_results = self.search_multi_line_string(contents)
        for search_result in search_results:
            search_result.file = sf
        return search_results

    def search_multi_line_string(self, s: str) -> List[SearchResult]:
        """Search a given searchable string possibly containing multiple newlines
           and return a list of SearchResult instances (without file_name)
        """
        search_results = []
        new_line_indices = get_new_line_indices(s)
        start_line_indices = [0] + [i + 1 for i in new_line_indices]
        end_line_indices = new_line_indices + [len(s) - 1]
        for p in self.settings.search_patterns:
            search_results.extend(
                self.search_multiline_string_for_pattern(s,
                                                         p,
                                                         start_line_indices,
                                                         end_line_indices))
        return search_results

    def get_lines_after_to_or_until(
            self, s: str, after_start_indices: List[int]):
        # lines_after_patterns = []
        if self.settings.lines_after_to_patterns:
            lines_after_patterns = self.settings.lines_after_to_patterns
        else:
            lines_after_patterns = self.settings.lines_after_until_patterns
        lines_after = []
        match_indices = {}
        for p in lines_after_patterns:
            m = p.search(s[after_start_indices[0]:])
            if m:
                match_indices[p] = m.start() + after_start_indices[0]
        if match_indices:
            first_index = min(x for x in list(match_indices.values()))
            for i, x in enumerate(after_start_indices):
                if x < first_index and i < len(after_start_indices) - 1:
                    lines_after.append(
                        s[after_start_indices[i]:after_start_indices[i + 1] - 1])
        if self.settings.lines_after_to_patterns:
            return lines_after
        else:
            return lines_after[:-1]

    def get_lines_after(self, s: str, after_start_indices: List[int], start_line_indices: List[int],
                        end_line_indices: List[int]):
        if self.do_lines_after_or_until():
            return self.get_lines_after_to_or_until(s, after_start_indices)
        return get_lines_at_indices(s, after_start_indices,
                                    start_line_indices, end_line_indices)

    def do_lines_after_or_until(self) -> bool:
        return len(self.settings.lines_after_to_patterns) > 0 or \
            len(self.settings.lines_after_until_patterns) > 0

    def do_lines_after(self) -> bool:
        return self.settings.lines_after > 0 or self.do_lines_after_or_until()

    def search_multiline_string_for_pattern(self, s: str, p, start_line_indices: List[int],
                                            end_line_indices: List[int]) -> List[SearchResult]:
        """Search a given searchable string possibly containing multiple newlines
           and a specific pattern and return a list of SearchResult instances
           (without file_name)
        """
        lines_before = deque()
        lines_after = deque()
        search_results = []
        matches = p.finditer(s)
        for m in matches:
            m_line_start_index = 0
            # m_line_end_index = len(s) - 1
            before_start_indices = [
                x for x in start_line_indices if x <= m.start()]
            before_line_count = 0
            if before_start_indices:
                m_line_start_index = before_start_indices.pop()
                before_line_count = len(before_start_indices)
                before_start_indices = before_start_indices[self.settings.lines_before * -1:]
            m_line_end_index = end_line_indices[start_line_indices.index(
                m_line_start_index)]
            line = s[m_line_start_index:m_line_end_index]
            if self.settings.lines_before and before_line_count:
                lines_before = get_lines_before(s,
                                                before_start_indices,
                                                start_line_indices,
                                                end_line_indices)
            if self.do_lines_after():
                after_start_indices = [
                    x for x in start_line_indices if x > m.start()]
                after_start_indices = after_start_indices[:self.settings.lines_after]
                lines_after = self.get_lines_after(s,
                                                   after_start_indices,
                                                   start_line_indices,
                                                   end_line_indices)
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
            if self.settings.first_match:
                break
        return search_results

    def lines_before_match(self, lines_before: Deque[str]) -> bool:
        return lines_match(lines_before,
                           self.settings.in_lines_before_patterns,
                           self.settings.out_lines_before_patterns)

    def lines_after_match(self, lines_after: Deque[str]) -> bool:
        if self.do_lines_after_or_until():
            return True
        return lines_match(lines_after,
                           self.settings.in_lines_after_patterns,
                           self.settings.out_lines_after_patterns)

    def __search_text_file_lines(self, sf: SearchFile, fo: TextIO):
        """Search in a given text file object by line and return the results"""
        search_results = self.search_line_iterator(fo)
        for search_result in search_results:
            search_result.file = sf
        return search_results

    def search_line_iterator(self, lines: TextIO) -> List[SearchResult]:
        """Consecutively search the lines of a line iterator and return results"""
        pattern_match_dict = {}
        line_num = 0
        lines_before = deque()
        lines_after = deque()
        results = []
        while True:
            if lines_after:
                line = lines_after.popleft()
            else:
                try:
                    line = next(lines)
                    line = line.rstrip('\r\n')
                except UnicodeDecodeError as e:
                    log('UnicodeDecodeError: {0!s}'.format(e))
                    break
                except StopIteration:
                    break
                except AttributeError as e:
                    log('AttributeError: {0!s}'.format(e))
                    break
            line_num += 1
            if self.settings.lines_after:
                while len(lines_after) < self.settings.lines_after:
                    try:
                        line_after = next(lines)
                        lines_after.append(line_after.rstrip('\r\n'))
                    except StopIteration:
                        break
            for p in self.settings.search_patterns:
                if self.settings.first_match and p in pattern_match_dict:
                    continue
                # find all matches for the line
                matchiter = p.finditer(line)
                while True:
                    try:
                        match = next(matchiter)
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
                            if self.settings.lines_after_to_patterns and \
                                    any_matches_any_pattern(lines_after,
                                                            self.settings.lines_after_to_patterns):
                                lines_after_to_match = True
                            if self.settings.lines_after_until_patterns and \
                                    any_matches_any_pattern(lines_after,
                                                            self.settings.lines_after_until_patterns):
                                lines_after_until_match = True
                            # if not read in more lines until a match or EOF
                            while not lines_after_to_match and \
                                    not lines_after_until_match:
                                try:
                                    next_line = next(lines).rstrip('\r\n')
                                    lines_after.append(next_line)
                                    if self.settings.lines_after_to_patterns and \
                                            matches_any_pattern(next_line,
                                                                self.settings.lines_after_to_patterns):
                                        lines_after_to_match = True
                                    elif self.settings.lines_after_until_patterns and \
                                            matches_any_pattern(next_line,
                                                                self.settings.lines_after_until_patterns):
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
                            sr_lines_after = list(lines_after)
                        search_result = \
                            SearchResult(pattern=p.pattern,
                                         line_num=line_num,
                                         line=line,
                                         match_start_index=match.start() + 1,
                                         match_end_index=match.end() + 1,
                                         lines_before=list(lines_before),
                                         lines_after=sr_lines_after)
                        results.append(search_result)
                        pattern_match_dict[p] = 1
            if self.settings.lines_before:
                if len(lines_before) == self.settings.lines_before:
                    lines_before.popleft()
                if len(lines_before) < self.settings.lines_before:
                    lines_before.append(line)
        return results

    def search_archive_file(self, sf: SearchFile) -> List[SearchResult]:
        """Search an archive (compressed) file"""
        ext = FileUtil.get_extension(sf.file_name)
        if not ext:
            return []
        if self.settings.debug:
            log('Searching {0} file {1}'.format(ext, sf))
        search_results = []
        if ext in ('zip', 'jar', 'war', 'ear'):
            # handle zip files
            try:
                search_results = self.search_zip_file(sf)
            except zipfile.BadZipfile as e:
                if not ext == 'ear':
                    log('BadZipfile: {0!s}: {1}'.format(e, sf))
        elif ext in ('bz2', 'tar', 'tgz', 'gz') and \
                tarfile.is_tarfile(sf.relative_path):
            # handle tar files
            try:
                search_results = self.search_tar_file(sf, ext)
            except Exception as e:
                msg = 'Exception while searching a tar file {0}: {1!s}'
                log(msg.format(sf, e))
        else:
            msg = 'Searching archive file type "{0}" is unsupported at this time'
            log(msg.format(ext))
        return search_results

    def search_zip_file(self, sf: SearchFile) -> List[SearchResult]:
        """Search an archive file compressed with zip"""
        zfo = zipfile.ZipFile(sf.relative_path, 'r')
        return self._search_zipfile_obj(sf, zfo)

    def _search_zipfile_obj(self, sf: SearchFile, zfo) -> List[SearchResult]:
        """Search a ZipFile object"""
        search_results = []
        zipinfos = zfo.infolist()
        for zipinfo in zipinfos:
            if zipinfo.file_size and self.is_search_file(zipinfo.file_name):
                zio = StringIO(zfo.read(zipinfo.file_name))
                zio.seek(0)
                nsf = SearchFile(containers=sf.containers + [sf.relative_path])
                nsf.path, nsf.file_name = os.path.split(zipinfo.file_name)
                search_results.extend(self.__search_zipinfo_obj(nsf, zio))
        return search_results

    def __search_zipinfo_obj(self, sf: SearchFile, zio) -> List[SearchResult]:
        """Search a ZipInfo object"""
        filetype = self.file_types.get_file_type(sf.file_name)
        if filetype == FileType.TEXT:
            return self.__search_text_file_obj(sf, zio)
        elif filetype == FileType.BINARY:
            return self.__search_binary_file_obj(sf, zio)
        return []

    def search_tar_file(self, sf, ext) -> List[SearchResult]:
        """Search a tar file"""
        search_results = []
        try:
            tfo = tarfile.open(sf.relative_path, 'r:' + ext)
            search_results = self.__search_tarfile_obj(sf, tfo)
            tfo.close()
        except tarfile.CompressionError as e:
            if not ext == 'tgz':
                msg = 'CompressionError while trying to open {0}: {1!s}'
                log(msg.format(sf, e))
        return search_results

    def __search_tarfile_obj(self, sf: SearchFile, tar) -> List[SearchResult]:
        """Search a tarfile object"""
        search_results = []
        for tarinfo in tar:
            if tarinfo.isfile() and self.is_search_file(tarinfo.name):
                tio = StringIO(tar.extractfile(tarinfo).read())
                tio.seek(0)
                nsf = SearchFile(containers=sf.containers + [sf.relative_path])
                nsf.path, nsf.file_name = os.path.split(tarinfo.name)
                search_results.extend(self.__search_tarinfo_obj(nsf, tio))
        return search_results

    def __search_tarinfo_obj(self, sf: SearchFile, tio) -> List[SearchResult]:
        """Search a tarinfo object"""
        filetype = self.file_types.get_file_type(sf.file_name)
        if filetype == FileType.TEXT:
            if self.settings.debug:
                log('searchable text file in tar: {0}'.format(sf.file_name))
            return self.__search_text_file_obj(sf, tio)
        elif filetype == FileType.BINARY:
            if self.settings.debug:
                log('searchable binary file in tar: {0}'.format(sf.file_name))
            return self.__search_binary_file_obj(sf, tio)
        return []


def matches_any_pattern(s: str, pattern_set: PatternSet):
    """Returns true if string s matches any pattern in pattern_set, else
       false"""
    return any(p.search(s) for p in pattern_set)


def any_matches_any_pattern(slist, pattern_set: PatternSet):
    """Returns true if any string in slist matches any pattern in
    pattern_set, else false"""
    for s in slist:
        if matches_any_pattern(s, pattern_set):
            return True
    return False


def get_new_line_indices(s: str) -> List[int]:
    indices = []
    for i, c in enumerate(s):
        if c == '\n':
            indices.append(i)
    return indices


def get_lines_at_indices(s: str, at_indices: List[int], start_line_indices: List[int],
                         end_line_indices: List[int]):
    if not at_indices:
        return []
    lines = []
    for i in at_indices:
        line = s[i:end_line_indices[start_line_indices.index(i)]]
        lines.append(line)
    return lines


def get_lines_before(s: str, before_start_indices: List[int], start_line_indices: List[int],
                     end_line_indices: List[int]):
    return get_lines_at_indices(
        s, before_start_indices, start_line_indices, end_line_indices)


def lines_match(lines, in_patterns: PatternSet, out_patterns: PatternSet):
    if (not in_patterns or
        any_matches_any_pattern(lines, in_patterns)) and \
            (not out_patterns or
             not any_matches_any_pattern(lines, out_patterns)):
        return True
    return False
