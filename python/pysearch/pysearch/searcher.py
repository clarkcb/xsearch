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
import io
import locale
from datetime import datetime
import os
from pathlib import Path
from collections import deque
from io import StringIO
from typing import Deque, Optional, TextIO

from pyfind import (FileResult, FileType, FileTypes, FileUtil, Finder, SortBy,
                    log, log_error, print_dir_results, print_file_results)

from .searchresult import SearchResult, SearchResultFormatter, SearchResultSorter
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
        self.file_types = self.finder.file_types

    def __validate_settings(self):
        """Assert required settings in SearchSettings instance"""
        assert self.settings.search_patterns, 'No search patterns defined'
        assert self.settings.lines_after >= 0, 'Invalid linesafter'
        assert self.settings.lines_before >= 0, 'Invalid linesbefore'
        assert self.settings.max_line_length >= 0, 'Invalid maxlinelength'
        try:
            codecs.lookup(self.settings.text_file_encoding)
        except LookupError:
            raise AssertionError('Invalid encoding: {}'.format(
                self.settings.text_file_encoding))

    async def search(self) -> list[SearchResult]:
        """Search files to find instances of searchpattern(s) starting from
           startpath"""
        # get the matching files via finder
        file_results = await self.finder.find()
        if self.settings.verbose:
            find_dirs = set([])
            for fr in file_results:
                d = ('!'.join(fr.containers) + '!' if fr.containers else '')
                d = d + str(fr.path.parent) if fr.path.parent else d + '.'
                find_dirs.add(d)
            find_dirs = sorted(list(find_dirs))
            log('\nDirectories to be searched ({0}):'.format(len(find_dirs)))
            for d in find_dirs:
                log(str(d))
            log('\n\nFiles to be searched ({0}):'.format(len(file_results)))
            for f in file_results:
                log(str(f))
            log("")

        # TODO: move to properties or settings
        batch_size = 250
        offset = 0
        search_results = []
        while offset < len(file_results):
            to_index = min(offset + batch_size, len(file_results))
            tasks = []
            for fr in file_results[offset:to_index]:
                tasks.append(asyncio.create_task(self.search_file(fr)))
            for coroutine in asyncio.as_completed(tasks):
                search_results.extend(await coroutine)
            offset += batch_size
        if len(search_results) > 1:
            search_result_sorter = SearchResultSorter(self.settings)
            return search_result_sorter.sort(search_results)
        return search_results

    async def search_contained_file(self, containers: list[Path], fr: FileResult) -> list[SearchResult]:
        """Search a contained file and return the results"""
        search_results = []
        file_bytes = self.get_archive_contained_file_bytes(containers, fr.path)
        if fr.file_type in self.file_types.TEXT_TYPES:
            try:
                search_results = self.search_string(file_bytes.decode(locale.getencoding()))
            except UnicodeDecodeError as e:
                search_results = self.search_string(file_bytes.decode('latin-1'))
            for search_result in search_results:
                search_result.file = fr
        elif fr.file_type == FileType.BINARY:
            search_results = self.search_binary_file(fr)
        return search_results

    async def search_file(self, fr: FileResult) -> list[SearchResult]:
        """Search in a file and return the results"""
        search_results = []
        if fr.containers:
            # TODO: need to handle searching files contained in archives
            containers = [Path(c) for c in fr.containers]
            search_results = await self.search_contained_file(containers, fr)
        elif fr.file_type in self.file_types.TEXT_TYPES:
            search_results = self.search_text_file(fr)
        elif fr.file_type == FileType.BINARY:
            search_results = self.search_binary_file(fr)
        elif fr.file_type == FileType.ARCHIVE and self.settings.search_archives:
            try:
                search_results = self.search_archive_file(fr)
            except IOError as e:
                log_error('IOError: {0!s}: {1!s}'.format(e, fr))
        else:
            # it's UNKNOWN
            if self.settings.verbose:
                log('Skipping file with unknown type: {0}'.format(fr))
        return search_results

    def search_binary_file(self, fr: FileResult) -> list[SearchResult]:
        """Search a binary file"""
        if self.settings.debug:
            log("Searching binary file {0!s}".format(fr.relative_path))
        search_results = []
        try:
            fo = open(fr.relative_path, mode='r', encoding='latin-1')
            search_results = self.__search_binary_file_obj(fr, fo)
            fo.close()
        except IOError as e:
            log_error('IOError: {0!s}: {1!s}'.format(e, fr))
        return search_results

    def __search_binary_file_obj(self, fr: FileResult, fo: TextIO) -> list[SearchResult]:
        """Search a binary file object"""
        contents = fo.read()
        search_results = self.__search_binary_contents(contents)
        for search_result in search_results:
            search_result.file = fr
        return search_results

    def __search_binary_contents(self, contents: str) -> list[SearchResult]:
        """Search a binary contents (single-byte encoded str)"""
        search_results = []
        for p in self.settings.search_patterns:
            matches = p.finditer(contents)
            for m in matches:
                search_result = SearchResult(pattern=p.pattern,
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

    def search_text_file(self, fr: FileResult) -> list[SearchResult]:
        """Search a text file"""
        if self.settings.debug:
            log("Searching text file {0!s}".format(fr.relative_path))
        search_results = []
        try:
            fo = open(fr.relative_path, mode='r',
                      encoding=self.settings.text_file_encoding)
            search_results = self.__search_text_file_obj(fr, fo)
            fo.close()
        except IOError as e:
            log_error('IOError: {0!s}: {1}'.format(e, fr))
        except UnicodeDecodeError as e:
            log_error('UnicodeDecodeError: {0!s}: {1}'.format(e, fr))
        return search_results

    def __search_text_file_obj(self, fr: FileResult, fo: TextIO) -> list[SearchResult]:
        """Search a text file object"""
        if self.settings.multi_line_search:
            contents = fo.read()
            return self.__search_text_file_contents(fr, contents)
        return self.__search_text_file_lines(fr, fo)

    def __search_text_file_contents(
            self, fr: FileResult, contents: str) -> list[SearchResult]:
        """Search a given text file object contents all at once
        """
        search_results = self.search_multi_line_string(contents)
        for search_result in search_results:
            search_result.file = fr
        return search_results

    def search_string(self, s: str) -> list[SearchResult]:
        """An alias for search_multi_line_string"""
        return self.search_multi_line_string(s)

    def search_multi_line_string(self, s: str) -> list[SearchResult]:
        """Search a given searchable string possibly containing multiple newlines
           and return a list of SearchResult instances (without file_name)
        """
        search_results = []
        new_line_indices = get_new_line_indices(s)
        start_line_indices = [0] + [i + 1 for i in new_line_indices]
        end_line_indices = new_line_indices + [len(s) - 1]
        for p in self.settings.search_patterns:
            search_results.extend(
                self.search_multi_line_string_for_pattern(s,
                                                          p,
                                                          start_line_indices,
                                                          end_line_indices))
        return search_results

    def get_lines_after_to_or_until(
            self, s: str, after_start_indices: list[int]):
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

    def get_lines_after(self, s: str, after_start_indices: list[int], start_line_indices: list[int],
                        end_line_indices: list[int]):
        if self.do_lines_after_or_until():
            return self.get_lines_after_to_or_until(s, after_start_indices)
        return get_lines_at_indices(s, after_start_indices,
                                    start_line_indices, end_line_indices)

    def do_lines_after_or_until(self) -> bool:
        return len(self.settings.lines_after_to_patterns) > 0 or \
            len(self.settings.lines_after_until_patterns) > 0

    def do_lines_after(self) -> bool:
        return self.settings.lines_after > 0 or self.do_lines_after_or_until()

    def search_multi_line_string_for_pattern(self, s: str, p, start_line_indices: list[int],
                                             end_line_indices: list[int]) -> list[SearchResult]:
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
                                         line_num=before_line_count + 1,
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

    def __search_text_file_lines(self, fr: FileResult, fo: TextIO):
        """Search in a given text file object by line and return the results"""
        search_results = self.search_line_iterator(fr, fo)
        for search_result in search_results:
            search_result.file = fr
        return search_results

    def search_line_iterator(self, fr: FileResult, lines: TextIO) -> list[SearchResult]:
        """Consecutively search the lines of a line iterator and return results"""
        pattern_match_dict = {}
        line_num = 0
        lines_before = deque()
        lines_after = deque()
        results = []
        stop_lines = False
        while not stop_lines:
            if lines_after:
                line = lines_after.popleft()
            else:
                try:
                    line = next(lines)
                    line = line.rstrip('\r\n')
                except UnicodeDecodeError as e:
                    log_error('UnicodeDecodeError: {0!s}: {1}'.format(e, fr))
                    break
                except StopIteration:
                    break
                except AttributeError as e:
                    log_error('AttributeError: {0!s}'.format(e))
                    break
            line_num += 1
            if self.settings.lines_after:
                while len(lines_after) < self.settings.lines_after:
                    try:
                        line_after = next(lines)
                        lines_after.append(line_after.rstrip('\r\n'))
                    except StopIteration:
                        break
            if self.settings.first_match and \
                len(pattern_match_dict.keys()) == len(self.settings.search_patterns):
                stop_lines = True
            for p in self.settings.search_patterns:
                if self.settings.first_match and p in pattern_match_dict:
                    continue
                # find all matches for the line
                matchiter = p.finditer(line)
                stop_matches = False
                while not stop_matches:
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
                        if self.settings.first_match:
                            stop_matches = True
            if self.settings.lines_before:
                if len(lines_before) == self.settings.lines_before:
                    lines_before.popleft()
                if len(lines_before) < self.settings.lines_before:
                    lines_before.append(line)
        return results

    def __search_zipinfo_obj(self, fr: FileResult, zio) -> list[SearchResult]:
        """Search a ZipInfo object"""
        filetype = self.file_types.get_file_type(fr.path.name)
        if filetype in FileTypes.TEXT_TYPES:
            return self.__search_text_file_obj(fr, zio)
        elif filetype == FileType.BINARY:
            return self.__search_binary_file_obj(fr, zio)
        return []

    def _search_zipfile_obj(self, fr: FileResult, file) -> list[SearchResult]:
        """Search a zip file object"""
        search_results = []
        try:
            with zipfile.ZipFile(file, 'r') as zfo:
                for zip_info in zfo.infolist():
                    if zip_info.is_dir() or not zip_info.file_size or not zip_info.filename:
                        continue
                    zip_info_path = Path(zip_info.filename)
                    if zip_info_path.parent and not self.finder.is_matching_dir(zip_info_path.parent):
                        continue
                    file_type = self.file_types.get_file_type(zip_info_path.name)
                    if file_type == FileType.ARCHIVE:
                        if not self.finder.is_matching_archive_file_name(zip_info_path.name):
                            continue
                    else:
                        if not self.finder.is_matching_file_type(file_type):
                            continue
                        if not self.finder.is_matching_file_name(zip_info_path.name):
                            continue
                        if self.settings.in_extensions or self.settings.out_extensions:
                            ext = FileUtil.get_extension(zip_info_path.name)
                            if not self.finder.is_matching_ext(ext):
                                continue
                        # Can't get stat of file in zip, but we can get the file_size and last_mod
                        if self.settings.need_stat():
                            file_size = zip_info.file_size
                            if file_size and not self.finder.is_matching_file_size(file_size):
                                continue
                            last_mod = zip_info.date_time
                            # convert last_mod from date tuple to float
                            last_mod = datetime(*last_mod).timestamp()
                            if last_mod and not self.finder.is_matching_last_mod(last_mod):
                                continue
                    nfr = FileResult(containers=fr.containers + [fr.relative_path],
                                     path=zip_info_path,
                                     file_type=file_type,
                                     stat=None)
                    zio = io.BytesIO(zfo.read(zip_info))
                    if file_type == FileType.TEXT:
                        # encoding defaults to locale.getencoding() if not specified
                        zio = io.TextIOWrapper(zio)
                    # zio = StringIO(zfo.read(zip_info))
                    search_results.extend(self.__search_zipinfo_obj(nfr, zio))
        except zipfile.BadZipFile as error:
            print('BadZipfile: {0!s}: {1}'.format(error, fr))
        return search_results

    def search_zip_file(self, fr: FileResult) -> list[SearchResult]:
        """Search an archive file compressed with zip"""
        return self._search_zipfile_obj(fr, fr.relative_path)

    def _get_zipfile_obj(self, file_path: Path):
        """Get a ZipFile object - must remember to close it manually"""
        try:
            zfo = zipfile.ZipFile(file_path, 'r')
        except zipfile.BadZipfile as e:
            log_error('BadZipfile: {0!s}: {1}'.format(e, file_path))
        else:
            return zfo
        return None

    # def _get_named_zipinfo_obj(self, zfo, name):
    #     """Get a ZipInfo object by name"""
    #     try:
    #         zio = zfo.getinfo(name)
    #     except KeyError as e:
    #         log_error('KeyError: {0!s}: {1}'.format(e, name))
    #         return None
    #     return zio

    def _get_zipfile_contained_file_obj(self, containers: list[Path], file_path: Path) -> Optional[zipfile.ZipInfo]:
        """Get a ZipInfo object contained in a ZipFile - must remember to close it manually"""
        if not containers:
            return None
        try:
            zfos: list[zipfile.ZipFile] = [zipfile.ZipFile(containers.pop(0), 'r')]
            while containers:
               zfos.insert(0, zipfile.ZipFile(zfos[0].open(containers.pop(0)), 'r'))
            zio = zfos[0].getinfo(str(file_path))
        except zipfile.BadZipFile as error:
            print('BadZipfile: {0!s}'.format(error))
        else:
            for zfo in zfos:
                zfo.close()
            return zio
        return None

    def _get_zipfile_contained_file_bytes(self, containers: list[Path], file_path: Path) -> bytes:
        """Get bytes of a ZipInfo object contained in a ZipFile"""
        if not containers:
            return b''
        try:
            zfos: list[zipfile.ZipFile] = [zipfile.ZipFile(containers.pop(0), 'r')]
            while containers:
               zfos.insert(0, zipfile.ZipFile(zfos[0].open(str(containers.pop(0))), 'r'))
            zio = zfos[0].getinfo(str(file_path))
        except zipfile.BadZipFile as error:
            print('BadZipfile: {0!s}'.format(error))
        else:
            info_bytes = zfos[0].read(zio)
            for zfo in zfos:
                zfo.close()
            return info_bytes
        return b''

    def __search_tarinfo_obj(self, fr: FileResult, tio) -> list[SearchResult]:
        """Search a tarinfo object"""
        file_type = self.file_types.get_file_type(fr.filename)
        if file_type in FileTypes.TEXT_TYPES:
            if self.settings.debug:
                log('searchable text file in tar: {0}'.format(fr.filename))
            return self.__search_text_file_obj(fr, tio)
        elif file_type == FileType.BINARY:
            if self.settings.debug:
                log('searchable binary file in tar: {0}'.format(fr.filename))
            return self.__search_binary_file_obj(fr, tio)
        return []

    def __search_tarfile_obj(self, fr: FileResult, tar) -> list[SearchResult]:
        """Search a tarfile object"""
        search_results = []
        for tarinfo in tar:
            if tarinfo.isfile() and self.finder.is_matching_file(tarinfo.name):
                tio = StringIO(tar.extractfile(tarinfo).read())
                tio.seek(0)
                nfr = FileResult(containers=fr.containers + [fr.relative_path])
                nfr.path, nfr.filename = os.path.split(tarinfo.name)
                search_results.extend(self.__search_tarinfo_obj(nfr, tio))
        return search_results

    def search_tar_file(self, fr: FileResult, ext) -> list[SearchResult]:
        """Search a tar file"""
        search_results = []
        try:
            tfo = tarfile.open(fr.relative_path, 'r:' + ext)
            search_results = self.__search_tarfile_obj(fr, tfo)
            tfo.close()
        except tarfile.CompressionError as e:
            if not ext == 'tgz':
                msg = 'CompressionError while trying to open {0}: {1!s}'
                log_error(msg.format(fr, e))
        return search_results

    def search_archive_file(self, fr: FileResult) -> list[SearchResult]:
        """Search an archive (compressed) file"""
        ext = FileUtil.get_extension(fr.path.name)
        if not ext:
            return []
        if self.settings.debug:
            log('Searching {0} file {1}'.format(ext, fr))
        search_results = []
        if ext in self.file_types.ZIPFILE_EXTENSIONS:
            # handle zip files
            try:
                search_results = self.search_zip_file(fr)
            except zipfile.BadZipfile as e:
                if not ext == 'ear':
                    log_error('BadZipfile: {0!s}: {1}'.format(e, fr))
        elif ext in self.file_types.TARFILE_EXTENSIONS and \
                tarfile.is_tarfile(fr.relative_path):
            # handle tar files
            try:
                search_results = self.search_tar_file(fr, ext)
            except Exception as e:
                msg = 'Exception while searching a tar file {0}: {1!s}'
                log_error(msg.format(fr, e))
        else:
            msg = 'Searching archive file type "{0}" is unsupported at this time'
            log(msg.format(ext))
        return search_results

    def get_archive_file_obj(self, file_path: Path):
        """Get an archive file object - must remember to close it manually"""
        ext = FileUtil.get_extension(file_path.name)
        if not ext:
            return None
        if ext in self.file_types.ZIPFILE_EXTENSIONS:
            return self._get_zipfile_obj(file_path)
        return None

    def get_archive_contained_file_obj(self, containers: list[Path], file_path: Path):
        """Get a file object contained in an archive - must remember to close it manually"""
        ext = FileUtil.get_extension(file_path.name)
        if not ext:
            return None
        if ext in self.file_types.ZIPFILE_EXTENSIONS:
            return self._get_zipfile_contained_file_obj(containers, file_path)
        return None

    def get_archive_contained_file_bytes(self, containers: list[Path], file_path: Path) -> bytes:
        """Get a file object contained in an archive - must remember to close it manually"""
        if not containers:
            return b''
        ext = FileUtil.get_extension(containers[0].name)
        if not ext:
            return b''
        if ext in self.file_types.ZIPFILE_EXTENSIONS:
            return self._get_zipfile_contained_file_bytes(containers, file_path)
        return b''


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


def get_new_line_indices(s: str) -> list[int]:
    indices = []
    for i, c in enumerate(s):
        if c == '\n':
            indices.append(i)
    return indices


def get_lines_at_indices(s: str, at_indices: list[int], start_line_indices: list[int],
                         end_line_indices: list[int]):
    if not at_indices:
        return []
    lines = []
    for i in at_indices:
        line = s[i:end_line_indices[start_line_indices.index(i)]]
        lines.append(line)
    return lines


def get_lines_before(s: str, before_start_indices: list[int], start_line_indices: list[int],
                     end_line_indices: list[int]):
    return get_lines_at_indices(
        s, before_start_indices, start_line_indices, end_line_indices)


def lines_match(lines, in_patterns: PatternSet, out_patterns: PatternSet):
    if (not in_patterns or
        any_matches_any_pattern(lines, in_patterns)) and \
            (not out_patterns or
             not any_matches_any_pattern(lines, out_patterns)):
        return True
    return False


def print_search_results(results: list[SearchResult], formatter: SearchResultFormatter):
    log(f'Search results ({len(results)}):')
    for r in results:
        s = formatter.format(r)
        try:
            log(s)
        except UnicodeEncodeError:
            log(repr(s))


def get_matching_file_results(search_results: list[SearchResult]) -> list[FileResult]:
    """Get list of files with matches"""
    file_set = {}
    files = []
    for r in search_results:
        if r.file:
            if str(r.file) not in file_set:
                files.append(r.file)
                file_set[str(r.file)] = True
    return files


def print_search_dir_results(search_results: list[SearchResult], formatter: SearchResultFormatter):
    """Print the dir results"""
    file_results = get_matching_file_results(search_results)
    print_dir_results(file_results, formatter.file_formatter)


def print_search_file_results(search_results: list[SearchResult], formatter: SearchResultFormatter):
    """Print the file results"""
    file_results = get_matching_file_results(search_results)
    print_file_results(file_results, formatter.file_formatter)


def get_matching_lines(search_results: list[SearchResult], settings: SearchSettings) -> list[str]:
    """Get list of lines with matches (unique if settings.unique_lines)"""
    lines = [r.line.lstrip() for r in search_results if r.line]
    if settings.unique_lines:
        lines = list(set(lines))
    return list(sorted(lines, key=lambda s: s.upper()))


def print_search_lines_results(search_results: list[SearchResult], formatter: SearchResultFormatter):
    """Print the lines results"""
    lines = get_matching_lines(search_results, formatter.settings)
    if lines:
        line_len = len(lines)
        if formatter.settings.unique_lines:
            msg = f'\nUnique matching lines ({line_len}):'
        else:
            msg = f'\nMatching lines ({line_len}):'
        log(msg)
        for line in lines:
            log(formatter.format_line(line))
    else:
        log('\nMatching lines: 0')
