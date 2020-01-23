/*
 * searcher.js
 *
 * performs the searching based on the given SearchSettings instance
 */

var assert = require('assert');
var fs = require('fs');
var path = require('path');

var common = require('./common.js');
var FileType = require('./filetype.js').FileType;
var FileTypes = require('./filetypes.js').FileTypes;
var FileUtil = require('./fileutil.js').FileUtil;
var SearchResult = require('./searchresult.js').SearchResult;

function Searcher(settings) {
    "use strict";
    let self = this;
    const _settings = settings;
    const _binaryEncoding = "latin1";
    // from https://github.com/nodejs/node/blob/master/lib/buffer.js
    const _supportedEncodings = ['utf-8', 'utf8', 'latin1', 'ascii', 'ucs2',  'ucs-2', 'utf16le', 'binary', 'base64', 'hex'];
    const _filetypes = new FileTypes();
    self.results = [];

    const validateSettings = function () {
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
            assert.ok(self.isSearchDir(_settings.startPath), 'Startpath does not match search settings');
        } else if (stat.isFile()) {
            assert.ok(self.filterFile(_settings.startPath), 'Startpath does not match search settings');
        } else {
            assert.ok(false, 'Startpath not readable file type');
        }
        assert.ok(_settings.searchPatterns.length, 'No search patterns defined');
        assert.ok(_supportedEncodings.indexOf(_settings.textFileEncoding) > -1, "Invalid encoding");
    };

    const matchesAnyElement = (s, elements) => elements.indexOf(s) > -1;

    const matchesAnyPattern = function (s, patterns) {
        return patterns.some((p, i, arr) => s.search(p) > -1);
    };

    const anyMatchesAnyPattern = function (ss, patterns) {
        return ss.some((s, i, arr) =>matchesAnyPattern(s, patterns));
    };

    self.isSearchDir = function (dir) {
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

    self.isSearchFile = function (file) {
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

    self.isArchiveSearchFile = function (file) {
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

    const getSearchDirs = function (startPath) {
        let searchDirs = [];
        let stats = fs.statSync(startPath);
        if (stats.isDirectory()) {
            if (self.isSearchDir(startPath)) {
                searchDirs.push(startPath);
                if (_settings.recursive) {
                    if (_settings.debug) {
                        common.log(`Getting list of directories to search under ${startPath}`);
                    }
                    [].push.apply(searchDirs, recGetSearchDirs(startPath));
                }
            } else {
                common.log("Warning: startPath does not match search criteria");
            }
        } else if (stats.isFile()) {
            let d = path.dirname(startPath);
            if (!d) d = ".";
            if (self.isSearchDir(d)) {
                searchDirs.push(d);
            } else {
                common.log("Warning: startPath's path does not match search criteria");
            }
        }
        return searchDirs;
    };

    const recGetSearchDirs = function (currentDir) {
        let searchDirs = [];
        fs.readdirSync(currentDir).map(f => {
            return path.join(currentDir, f);
        }).filter(f => {
            return fs.statSync(f).isDirectory() && self.isSearchDir(f);
        }).forEach(f => {
            searchDirs.push(f);
            [].push.apply(searchDirs, recGetSearchDirs(f));
        });
        return searchDirs;
    };

    const handleFsError = function (err) {
        if (err.errno === 34 && err.code === "ENOENT") {
            // this error seems to occur when the file is a soft link
            // to a non-existent file
        } else {
            common.log(err);
            process.exit(1);
        }
    };

    self.filterFile = function (f) {
        if (_filetypes.isArchiveFile(f)) {
            return (_settings.searchArchives && self.isArchiveSearchFile(f));
        }
        return (!_settings.archivesOnly && self.isSearchFile(f));
    };

    const getSearchFilesForDirectory = function (dir) {
        return fs.readdirSync(dir).map(f => {
            return path.join(dir, f);
        }).filter(f => {
            return fs.statSync(f).isFile() && self.filterFile(f);
        });
    };

    const getSearchFiles = function (searchDirs) {
        let searchFiles = [];
        let stats = fs.statSync(_settings.startPath);
        if (stats.isDirectory()) {
            searchDirs.forEach(d => {
                searchFiles.push.apply(searchFiles, getSearchFilesForDirectory(d));
            });
        } else if (stats.isFile()) {
            searchFiles.push(_settings.startPath);
        }
        return searchFiles;
    };

    self.search = function () {
        if (_settings.verbose) {
            common.log("Search initiated");
        }

        // get the search dirs
        let dirs = [];
        dirs.push.apply(dirs, getSearchDirs(_settings.startPath));
        if (_settings.verbose) {
            common.log("\nDirectories to be searched " + `(${dirs.length}):`);
            dirs.forEach(d => common.log(d));
        }

        // get the search files
        let files = getSearchFiles(dirs);
        if (_settings.verbose) {
            common.log("\nFiles to be searched " + `(${files.length}):`);
            files.forEach(f => common.log(f));
            common.log("");
        }

        // search the files
        files.forEach(f => searchFile(f));

        if (_settings.verbose) {
            common.log("Search complete.");
        }
    };

    const searchFile = function (filepath) {
        let filetype = _filetypes.getFileType(filepath);
        if (filetype === FileType.TEXT) {
            searchTextFile(filepath);
        } else if (filetype === FileType.BINARY) {
            searchBinaryFile(filepath);
        }
    };

    const searchBinaryFile = function (filepath) {
        if (_settings.verbose) {
            common.log(`Searching binary file: "${filepath}"`);
        }
        let contents = FileUtil.getFileContents(filepath, _binaryEncoding);
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
                    filepath,
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

    const searchTextFile = function (filepath) {
        if (_settings.verbose) {
            common.log(`Searching text file ${filepath}`);
        }
        if (_settings.multilineSearch) {
            searchTextFileContents(filepath);
        } else {
            searchTextFileLines(filepath);
        }
    };

    const searchTextFileContents = function (filepath) {
        let contents = FileUtil.getFileContents(filepath, _settings.textFileEncoding);
        let results = self.searchMultiLineString(contents);
        results.forEach(r => {
            let resultWithFilepath =
                new SearchResult(r.pattern, filepath, r.linenum,
                    r.matchStartIndex, r.matchEndIndex, r.line,
                    r.linesBefore, r.linesAfter);
            addSearchResult(resultWithFilepath);
        });
    };

    const getNewLineIndices = function (s) {
        let indices = [];
        for (let i = 0; i < s.length; i++) {
            if (s.charAt(i) == "\n") {
                indices.push(i);
            }
        }
        return indices;
    };

    const getLinesAtIndices = function(s, atIndices, startLineIndices, endLineIndices) {
        if (atIndices.length === 0)
            return [];
        let lines = [];
        atIndices.forEach(i => {
            let line = s.substring(i, endLineIndices[startLineIndices.indexOf(i)]);
            lines.push(line);
        });
        return lines;
    };

    const getLinesBefore = function(s, beforeStartIndices, startLineIndices, endLineIndices) {
        return getLinesAtIndices(s, beforeStartIndices, startLineIndices, endLineIndices);
    };

    const getLinesAfter = function(s, afterStartIndices, startLineIndices, endLineIndices) {
        return getLinesAtIndices(s, afterStartIndices, startLineIndices, endLineIndices);
    };

    const getLessThanOrEqual = matchVal => function(i) { return i <= matchVal; };

    const getGreaterThan = matchVal =>function(i) { return i > matchVal; };

    const plusOne = i => i + 1;

    self.searchMultiLineString = function (s) {
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

    const linesMatch = function(lines, inPatterns, outPatterns) {
        return ((inPatterns.length === 0 || anyMatchesAnyPattern(lines, inPatterns)) &&
               (outPatterns.length === 0 || ! anyMatchesAnyPattern(lines, outPatterns)));
    };

    const linesBeforeMatch = function(linesBefore) {
        return linesMatch(linesBefore, _settings.inLinesBeforePatterns,
            _settings.outLinesBeforePatterns);
    };

    const linesAfterMatch = function(linesAfter) {
        return linesMatch(linesAfter, _settings.inLinesAfterPatterns,
            _settings.outLinesAfterPatterns);
    };

    const searchTextFileLines = function (filepath) {
        let lines = FileUtil.getFileLines(filepath, _settings.textFileEncoding);
        let results = self.searchLines(lines);
        results.forEach(r => {
            let resultWithFilepath =
                new SearchResult(r.pattern, filepath, r.linenum,
                    r.matchStartIndex, r.matchEndIndex, r.line,
                    r.linesBefore, r.linesAfter);
            addSearchResult(resultWithFilepath);
        });
    };

    // return results so that filepath can be added to them
    self.searchLines = function (lines) {
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
                if (linesBefore.length == _settings.linesBefore)
                    linesBefore.shift();
                if (linesBefore.length < _settings.linesBefore)
                    linesBefore.push(line);
            }
        }
        return results;
    };

    const addSearchResult = function (result) {
        self.results.push(result);
    };

    const cmpSearchResults = function(r1, r2) {
        const pathCmp = path.dirname(r1.filename).localeCompare(path.dirname(r2.filename));
        if (pathCmp === 0) {
            const fileCmp = path.basename(r1.filename).localeCompare(path.basename(r2.filename));
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

    self.printSearchResults = function () {
        // first sort the results
        self.results.sort(cmpSearchResults);
        common.log("\nSearch results " + `(${self.results.length}):`);
        self.results.forEach(r => common.log(r.toString()));
    };

    self.getMatchingDirs = function () {
        const dirs = self.results.map(r => path.dirname(r.filename));
        return common.setFromArray(dirs);
    };

    self.printMatchingDirs = function () {
        const dirs = self.getMatchingDirs();
        common.log("\nDirectories with matches " + `(${dirs.length}):`);
        dirs.forEach(d => common.log(d));
    };

    self.getMatchingFiles = function () {
        const files = self.results.map(r => r.filename);
        return common.setFromArray(files);
    };

    self.printMatchingFiles = function () {
        const files = self.getMatchingFiles();
        common.log("\nFiles with matches " + `(${files.length}):`);
        files.forEach(f => common.log(f));
    };

    self.getMatchingLines = function () {
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

    self.printMatchingLines = function () {
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
