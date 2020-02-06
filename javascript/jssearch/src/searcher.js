/*
 * searcher.js
 *
 * performs the searching based on the given SearchSettings instance
 */

const assert = require('assert');
const fs = require('fs');
const path = require('path');

const common = require('./common.js');
const FileType = require('./filetype.js').FileType;
const FileTypes = require('./filetypes.js').FileTypes;
const FileUtil = require('./fileutil.js').FileUtil;
const SearchFile = require('./searchfile.js').SearchFile;
const SearchResult = require('./searchresult.js').SearchResult;

function Searcher(settings) {
    "use strict";
    let self = this;
    const _settings = settings;
    const _binaryEncoding = "latin1";
    // from https://github.com/nodejs/node/blob/master/lib/buffer.js
    const _supportedEncodings = ['utf-8', 'utf8', 'latin1', 'ascii', 'ucs2',  'ucs-2', 'utf16le',
        'binary', 'base64', 'hex'];
    const _filetypes = new FileTypes();
    self.results = [];

    const validateSettings = () => {
        assert.ok(_settings.startPath, 'Startpath not defined');
        assert.ok(fs.existsSync(_settings.startPath), 'Startpath not found');
        try {
            fs.accessSync(_settings.startPath, fs.constants.R_OK);
        } catch (accessErr) {
            if (accessErr.code === "EACCES") {
                assert.ok(false, 'Startpath not readable');
            }
            throw accessErr;
        }
        let stat = fs.lstatSync(_settings.startPath);
        if (stat.isDirectory()) {
            assert.ok(self.isSearchDir(_settings.startPath),
                'Startpath does not match search settings');
        } else if (stat.isFile()) {
            assert.ok(self.filterFile(_settings.startPath),
                'Startpath does not match search settings');
        } else {
            assert.ok(false, 'Startpath not readable file type');
        }
        assert.ok(_settings.searchPatterns.length, 'No search patterns defined');
        assert.ok(_supportedEncodings.indexOf(_settings.textFileEncoding) > -1,
            "Invalid encoding");
        assert.ok(_settings.linesBefore > -1, "Invalid linesbefore");
        assert.ok(_settings.linesAfter > -1, "Invalid linesafter");
        assert.ok(_settings.maxLineLength > -1, "Invalid maxlinelength");
    };

    const matchesAnyElement = (s, elements) => elements.indexOf(s) > -1;

    const matchesAnyPattern = (s, patterns) => {
        return patterns.some((p, i, arr) => s.search(p) > -1);
    };

    const anyMatchesAnyPattern = (ss, patterns) => {
        return ss.some((s, i, arr) =>matchesAnyPattern(s, patterns));
    };

    self.isSearchDir = (dir) => {
        if (FileUtil.isDotDir(dir)) {
            return true;
        }
        if (_settings.excludeHidden) {
            let nonDotElems = dir.split(path.sep).filter(p => !matchesAnyElement(p, ['.','..']));
            if (nonDotElems.length === 0) {
                return true;
            }
            if (nonDotElems.some((p, i, arr) => FileUtil.isHidden(p))) {
                return false;
            }
        }
        if (_settings.inDirPatterns.length && !matchesAnyPattern(dir,
            _settings.inDirPatterns)) {
            return false;
        }
        return !(_settings.outDirPatterns.length && matchesAnyPattern(dir,
            _settings.outDirPatterns));
    };
    // can validate now that isSearchDir is defined
    validateSettings();

    self.isSearchFile = (file) => {
        if (FileUtil.isHidden(file) && _settings.excludeHidden) {
            return false;
        }
        let ext = FileUtil.getExtension(file);
        if ((_settings.inExtensions.length &&
            !matchesAnyElement(ext, _settings.inExtensions))
            || (_settings.outExtensions.length &&
                matchesAnyElement(ext, _settings.outExtensions))
            || (_settings.inFilePatterns.length &&
                !matchesAnyPattern(file, _settings.inFilePatterns))
            || (_settings.outFilePatterns.length &&
                matchesAnyPattern(file, _settings.outFilePatterns))) {
            return false;
        }
        let filetype = _filetypes.getFileType(file);
        return !((_settings.inFileTypes.length &&
            !matchesAnyElement(filetype, _settings.inFileTypes))
            || (_settings.outFileTypes.length &&
                matchesAnyElement(filetype, _settings.outFileTypes)));
    };

    self.isArchiveSearchFile = (file) => {
        if (FileUtil.isHidden(file) && _settings.excludeHidden) {
            return false;
        }
        let ext = FileUtil.getExtension(file);
        if (_settings.inArchiveExtensions.length &&
            !matchesAnyElement(ext, _settings.inArchiveExtensions)) {
            return false;
        }
        if (_settings.outArchiveExtensions.length &&
            matchesAnyElement(ext, _settings.outArchiveExtensions)) {
            return false;
        }
        if (_settings.inArchiveFilePatterns.length &&
            !matchesAnyPattern(file, _settings.inArchiveFilePatterns)) {
            return false;
        }
        return !(_settings.outArchiveFilePatterns.length &&
        matchesAnyPattern(file, _settings.outArchiveFilePatterns));
    };

    const getSearchFiles = (startPath) => {
        let searchFiles = [];
        let stats = fs.statSync(startPath);
        if (stats.isDirectory()) {
            if (self.isSearchDir(startPath)) {
                if (_settings.recursive) {
                    [].push.apply(searchFiles, recGetSearchFiles(startPath));
                } else {
                    fs.readdirSync(startPath).map(f => {
                        return path.join(startPath, f);
                    }).forEach(f => {
                        let stats = fs.statSync(f);
                        if (stats.isFile() && self.filterFile(f)) {
                            const filename = path.basename(startPath);
                            const filetype = _filetypes.getFileType(filename);
                            const sf = new SearchFile(d, filename, filetype);
                            searchFiles.push(sf);
                        }
                    });
                }
            } else {
                common.log("Warning: startPath does not match search criteria");
            }
        } else if (stats.isFile()) {
            let d = path.dirname(startPath);
            if (!d) d = ".";
            if (self.isSearchDir(d) && self.filterFile(startPath)) {
                const filename = path.basename(startPath);
                const filetype = _filetypes.getFileType(filename);
                const sf = new SearchFile(d, filename, filetype);
                searchFiles.push(sf);
            } else {
                common.log("Warning: startPath does not match search criteria");
            }
        }
        return searchFiles;
    };

    const recGetSearchFiles = (currentDir) => {
        let searchDirs = [];
        let searchFiles = [];
        fs.readdirSync(currentDir).map(f => {
            return path.join(currentDir, f);
        }).forEach(f => {
            let stats = fs.statSync(f);
            if (stats.isDirectory() && self.isSearchDir(f)) {
                searchDirs.push(f);
            } else if (stats.isFile() && self.filterFile(f)) {
                const pathname = path.dirname(f);
                const filename = path.basename(f);
                const filetype = _filetypes.getFileType(filename);
                const sf = new SearchFile(pathname, filename, filetype);
                searchFiles.push(sf);
            }
        });
        searchDirs.forEach(d => {
            [].push.apply(searchFiles, recGetSearchFiles(d));
        });
        return searchFiles;
    };

    self.filterFile = (f) => {
        if (_filetypes.isArchiveFile(f)) {
            return (_settings.searchArchives && self.isArchiveSearchFile(f));
        }
        return (!_settings.archivesOnly && self.isSearchFile(f));
    };

    self.search = () => {
        // get the search files
        let searchfiles = getSearchFiles(_settings.startPath);
        if (_settings.verbose) {
            let dirs = searchfiles.map(sf => sf.pathname);
            dirs = common.setFromArray(dirs);
            dirs.sort();
            common.log("\nDirectories to be searched " + `(${dirs.length}):`);
            dirs.forEach(d => common.log(d));

            common.log("\nFiles to be searched " + `(${searchfiles.length}):`);
            searchfiles.forEach(sf => common.log(sf.relativePath()));
            common.log("");
        }

        // search the files
        searchfiles.forEach(f => searchFile(f));

        if (_settings.verbose) {
            common.log("Search complete.");
        }
    };

    const searchFile = (searchfile) => {
        switch (searchfile.filetype) {
            case FileType.CODE:
            case FileType.TEXT:
            case FileType.XML:
                searchTextFile(searchfile);
                break;
            case FileType.BINARY:
                searchBinaryFile(searchfile);
                break;
            default:
                // TODO: add message about unsupported filetype
                break;
        }
    };

    const searchBinaryFile = (searchfile) => {
        if (_settings.verbose) {
            common.log(`Searching binary file: "${searchfile}"`);
        }
        let contents = FileUtil.getFileContents(searchfile.relativePath(), _binaryEncoding);
        let pattern = '';
        let patternResults = {};
        _settings.searchPatterns.forEach(p => {
            pattern = new RegExp(p.source, "g");
            if (_settings.firstMatch && (pattern.source in patternResults)) {
                return;
            }
            let match = pattern.exec(contents);
            while (match) {
                addSearchResult(new SearchResult(
                    pattern,
                    searchfile,
                    0,
                    match.index+1,
                    pattern.lastIndex+1,
                    null,
                    [],
                    []));
                if (_settings.firstMatch) {
                    patternResults[pattern.source] = 1;
                    break;
                }
                match = pattern.exec(contents);
            }
        });
    };

    const searchTextFile = (searchfile) => {
        if (_settings.verbose) {
            common.log(`Searching text file ${searchfile}`);
        }
        if (_settings.multilineSearch) {
            searchTextFileContents(searchfile);
        } else {
            searchTextFileLines(searchfile);
        }
    };

    const searchTextFileContents = (searchfile) => {
        let contents = FileUtil.getFileContents(searchfile.relativePath(), _settings.textFileEncoding);
        let results = self.searchMultiLineString(contents);
        results.forEach(r => {
            let resultWithFilepath =
                new SearchResult(r.pattern, searchfile, r.linenum,
                    r.matchStartIndex, r.matchEndIndex, r.line,
                    r.linesBefore, r.linesAfter);
            addSearchResult(resultWithFilepath);
        });
    };

    const getNewLineIndices = (s) => {
        let indices = [];
        for (let i = 0; i < s.length; i++) {
            if (s.charAt(i) === "\n") {
                indices.push(i);
            }
        }
        return indices;
    };

    const getLinesAtIndices = (s, atIndices, startLineIndices, endLineIndices) => {
        if (atIndices.length === 0)
            return [];
        let lines = [];
        atIndices.forEach(i => {
            let line = s.substring(i, endLineIndices[startLineIndices.indexOf(i)]);
            lines.push(line);
        });
        return lines;
    };

    const getLinesBefore = (s, beforeStartIndices, startLineIndices, endLineIndices) => {
        return getLinesAtIndices(s, beforeStartIndices, startLineIndices, endLineIndices);
    };

    const getLinesAfter = (s, afterStartIndices, startLineIndices, endLineIndices) => {
        return getLinesAtIndices(s, afterStartIndices, startLineIndices, endLineIndices);
    };

    const getLessThanOrEqual = matchVal => (i) => { return i <= matchVal; };

    const getGreaterThan = matchVal => (i) => { return i > matchVal; };

    const plusOne = i => i + 1;

    self.searchMultiLineString = (s) => {
        let patternResults = {};
        let linesBefore = [];
        let linesAfter = [];
        let results = [];
        let newLineIndices = getNewLineIndices(s);
        let startLineIndices = [0].concat(newLineIndices.map(plusOne));
        let endLineIndices = newLineIndices.concat([s.length - 1]);
        _settings.searchPatterns.forEach(p => {
            let pattern = new RegExp(p.source, "g");
            let match = pattern.exec(s);
            let stop = false;
            while (match && !stop) {
                if (_settings.firstMatch && pattern.source in patternResults) {
                    stop = true;
                    continue;
                }
                let lessOrEqual = getLessThanOrEqual(match.index);
                let greaterThan = getGreaterThan(match.index);
                let lineStartIndex = 0;
                let lineEndIndex = s.length - 1;
                let beforeLineCount = 0;
                let beforeStartIndices = startLineIndices.filter(lessOrEqual);
                if (beforeStartIndices.length > 0) {
                    lineStartIndex = beforeStartIndices.pop();
                    beforeLineCount = beforeStartIndices.length;
                    if (beforeStartIndices.length > _settings.linesBefore) {
                        beforeStartIndices = beforeStartIndices.slice(
                            beforeStartIndices.length - _settings.linesBefore);
                    }
                }
                lineEndIndex = endLineIndices[startLineIndices.indexOf(lineStartIndex)];
                let line = s.substring(lineStartIndex, lineEndIndex);
                if (_settings.linesBefore && beforeLineCount) {
                    linesBefore = getLinesBefore(s, beforeStartIndices,
                        startLineIndices, endLineIndices);
                }
                if (_settings.linesAfter) {
                    let afterStartIndices = startLineIndices.filter(greaterThan);
                    if (afterStartIndices.length > _settings.linesAfter) {
                        afterStartIndices = afterStartIndices.slice(0,
                            _settings.linesAfter);
                    }
                    linesAfter = getLinesAfter(s, afterStartIndices,
                        startLineIndices, endLineIndices);
                }
                let matchStartIndex = match.index - lineStartIndex + 1;
                let matchEndIndex = pattern.lastIndex - lineStartIndex + 1;
                if ((_settings.linesBefore === 0 || linesBeforeMatch(linesBefore)) &&
                    (_settings.linesAfter === 0 || linesAfterMatch(linesAfter))) {
                    let searchResult = new SearchResult(
                        pattern,
                        '',
                        beforeLineCount+1,
                        matchStartIndex,
                        matchEndIndex,
                        line,
                        [].concat(linesBefore),
                        [].concat(linesAfter));
                    results.push(searchResult);
                    if (!(pattern.source in patternResults)) {
                        patternResults[pattern.source] = 1;
                    }
                }
                match = pattern.exec(s);
            }
        });
        return results;
    };

    const linesMatch = (lines, inPatterns, outPatterns) => {
        return ((inPatterns.length === 0 || anyMatchesAnyPattern(lines, inPatterns)) &&
               (outPatterns.length === 0 || ! anyMatchesAnyPattern(lines, outPatterns)));
    };

    const linesBeforeMatch = (linesBefore) => {
        return linesMatch(linesBefore, _settings.inLinesBeforePatterns,
            _settings.outLinesBeforePatterns);
    };

    const linesAfterMatch = (linesAfter) => {
        return linesMatch(linesAfter, _settings.inLinesAfterPatterns,
            _settings.outLinesAfterPatterns);
    };

    const searchTextFileLines = (searchfile) => {
        let lines = FileUtil.getFileLines(searchfile.relativePath(), _settings.textFileEncoding);
        let results = self.searchLines(lines);
        results.forEach(r => {
            let resultWithFilepath =
                new SearchResult(r.pattern, searchfile, r.linenum,
                    r.matchStartIndex, r.matchEndIndex, r.line,
                    r.linesBefore, r.linesAfter);
            addSearchResult(resultWithFilepath);
        });
    };

    // return results so that filepath can be added to them
    self.searchLines = (lines) => {
        let linenum = 0;
        let pattern;
        let linesBefore = [];
        let linesAfter = [];
        let results = [];
        let patternResults = {};
        while (true) {
            if (Object.keys(patternResults).length === _settings.searchPatterns.length) {
                break;
            }
            let line = "";
            if (linesAfter.length > 0) {
                line = linesAfter.shift();
            } else if (lines.length > 0) {
                line = lines.shift();
            } else {
                break;
            }
            linenum += 1;
            if (_settings.linesAfter > 0) {
                while (linesAfter.length < _settings.linesAfter && lines.length > 0) {
                    linesAfter.push(lines.shift());
                }
            }
            _settings.searchPatterns.forEach(p => {
                pattern = new RegExp(p.source, "g");
                let match = pattern.exec(line);
                while (match) {
                    if ((_settings.linesBefore === 0 || linesBeforeMatch(linesBefore)) &&
                        (_settings.linesAfter === 0 || linesAfterMatch(linesAfter))) {
                        results.push(new SearchResult(
                            pattern,
                            '',
                            linenum,
                            match.index+1,
                            pattern.lastIndex+1,
                            line,
                            [].concat(linesBefore),
                            [].concat(linesAfter)));
                        if (_settings.firstMatch) {
                            patternResults[pattern.source] = 1;
                            break;
                        }
                    }
                    match = pattern.exec(line);
                }
            });
            if (_settings.linesBefore > 0) {
                if (linesBefore.length === _settings.linesBefore)
                    linesBefore.shift();
                if (linesBefore.length < _settings.linesBefore)
                    linesBefore.push(line);
            }
        }
        return results;
    };

    const addSearchResult = (result) => {
        self.results.push(result);
    };

    const cmpSearchResults = (r1, r2) => {
        const pathCmp = r1.file.pathname.localeCompare(r2.file.pathname);
        if (pathCmp === 0) {
            const fileCmp = path.basename(r1.file.filename).localeCompare(path.basename(r2.file.filename));
            if (fileCmp === 0) {
                if (r1.linenum === r2.linenum) {
                    return r1.matchStartIndex - r2.matchStartIndex;
                }
                return r1.linenum - r2.linenum;
            }
            return fileCmp;
        }
        return pathCmp;
    };

    self.printSearchResults = () => {
        // first sort the results
        self.results.sort(cmpSearchResults);
        common.log("\nSearch results " + `(${self.results.length}):`);
        self.results.forEach(r => common.log(r.toString()));
    };

    self.getMatchingDirs = () => {
        const dirs = self.results.map(r => path.dirname(r.filename));
        return common.setFromArray(dirs);
    };

    self.printMatchingDirs = () => {
        const dirs = self.getMatchingDirs();
        common.log("\nDirectories with matches " + `(${dirs.length}):`);
        dirs.forEach(d => common.log(d));
    };

    self.getMatchingFiles = () => {
        const files = self.results.map(r => r.filename);
        return common.setFromArray(files);
    };

    self.printMatchingFiles = () => {
        const files = self.getMatchingFiles();
        common.log("\nFiles with matches " + `(${files.length}):`);
        files.forEach(f => common.log(f));
    };

    self.getMatchingLines = () => {
        let lines = self.results.filter(r => r.linenum > 0).map(r => r.line.trim());
        if (_settings.uniqueLines) {
            lines = common.setFromArray(lines);
        }
        lines.sort((a, b) => {
            if (a.toUpperCase() === b.toUpperCase())
                return 0;
            return a.toUpperCase() < b.toUpperCase() ? -1 : 1;
        });
        return lines;
    };

    self.printMatchingLines = () => {
        const lines = self.getMatchingLines();
        let hdrText;
        if (_settings.uniqueLines)
            hdrText = "\nUnique lines with matches " + `(${lines.length}):`;
        else
            hdrText = "\nLines with matches " + `(${lines.length}):`;
        common.log(hdrText);
        lines.forEach(l => common.log(l));
    };
}

exports.Searcher = Searcher;
