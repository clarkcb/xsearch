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
    var self = this;
    var _settings = settings;
    var _filetypes = new FileTypes();
    self.results = [];

    var validateSettings = function () {
        assert.ok(_settings.startPath, 'Startpath not defined');
        assert.ok(fs.existsSync(_settings.startPath), 'Startpath not found');
        assert.ok(self.isSearchDir(_settings.startPath), 'Startpath does not match search settings');
        assert.ok(_settings.searchPatterns.length, 'No search patterns defined');
    };

    var matchesAnyElement = function (s, elements) {
        return elements.indexOf(s) > -1;
    };

    var matchesAnyPattern = function (s, patterns) {
        for (var i = 0; i < patterns.length; i++) {
            var pattern = patterns[i];
            var match = pattern.exec(s);
            if (match) {
                return true;
            }
        }
        return false;
    };

    var anyMatchesAnyPattern = function (ss, patterns) {
        for (var i = 0; i < ss.length; i++) {
            if (matchesAnyPattern(ss[i], patterns)) {
                return true;
            }
        }
        return false;
    };

    self.isSearchDir = function (dir) {
        if (FileUtil.isDotDir(dir)) {
            return true;
        }
        var pathElems = dir.split(path.sep);
        if (_settings.excludeHidden) {
            for (var i = 0; i < pathElems.length; i++) {
                if (!matchesAnyElement(pathElems[i], ['.','..']) &&
                    FileUtil.isHidden(pathElems[i])) {
                    return false;
                }
            }
        }
        if (_settings.inDirPatterns.length && !matchesAnyPattern(dir,
            _settings.inDirPatterns)) {
            return false;
        }
        if (_settings.outDirPatterns.length && matchesAnyPattern(dir,
            _settings.outDirPatterns)) {
            return false;
        }
        return true;
    };
    // can validate now that isSearchDir is defined
    validateSettings();

    self.isSearchFile = function (file) {
        if (FileUtil.isHidden(file) && _settings.excludeHidden) {
            return false;
        }
        var ext = FileUtil.getExtension(file);
        if (_settings.inExtensions.length &&
            !matchesAnyElement(ext, _settings.inExtensions)) {
            return false;
        }
        if (_settings.outExtensions.length &&
            matchesAnyElement(ext, _settings.outExtensions)) {
            return false;
        }
        if (_settings.inFilePatterns.length &&
            !matchesAnyPattern(file, _settings.inFilePatterns)) {
            return false;
        }
        if (_settings.outFilePatterns.length &&
            matchesAnyPattern(file, _settings.outFilePatterns)) {
            return false;
        }
        return true;
    };

    self.isArchiveSearchFile = function (file) {
        if (FileUtil.isHidden(file) && _settings.excludeHidden) {
            return false;
        }
        var ext = FileUtil.getExtension(file);
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
        if (_settings.outArchiveFilePatterns.length &&
            matchesAnyPattern(file, _settings.outArchiveFilePatterns)) {
            return false;
        }
        return true;
    };

    var getSearchDirs = function (startPath) {
        var searchDirs = [];
        var stats = fs.statSync(startPath);
        if (stats.isDirectory()) {
            if (_settings.debug) {
                common.log("Getting list of directories to search under {0}".format(startPath));
            }
            searchDirs.push(startPath);
            if (_settings.recursive) {
                searchDirs.push.apply(searchDirs, recGetSearchDirs(startPath));
            }
        } else if (stats.isFile()) {
            var d = path.dirname(startPath);
            if (!d) d = ".";
            searchDirs.push(d);
        }
        return searchDirs;
    };

    var handleFsError = function (err) {
        if (err.errno === 34 && err.code === "ENOENT") {
            // this error seems to occur when the file is a soft link
            // to a non-existent file
        } else {
            common.log(err);
            process.exit(1);
        }
    };

    var getSubDirs = function (dir) {
        var subDirs = [];
        var childItems = fs.readdirSync(dir);
        for (var i=0; i < childItems.length; i++) {
            var filepath = path.join(dir, childItems[i]);
            try {
                var stats = fs.statSync(filepath);
                if (stats.isDirectory()) {
                    subDirs.push(filepath);
                }
            } catch (err) {
                handleFsError(err);
            }
        }
        return subDirs;
    };

    var recGetSearchDirs = function (currentDir) {
        var searchDirs = [];
        var subDirs = getSubDirs(currentDir);
        for (var i=0; i < subDirs.length; i++) {
            if (self.isSearchDir(subDirs[i])) {
                searchDirs.push(subDirs[i]);
            }
            searchDirs.push.apply(searchDirs, recGetSearchDirs(subDirs[i]));
        }
        return searchDirs;
    };

    var getFilesForDirectory = function (dir) {
        var files = [];
        var childItems = fs.readdirSync(dir);
        for (var i=0; i < childItems.length; i++) {
            var filepath = path.join(dir, childItems[i]);
            try {
                var stats = fs.statSync(filepath);
                if (stats.isFile()) {
                    files.push(filepath);
                }
            } catch (err) {
                handleFsError(err);
            }
        }
        return files;
    };

    self.filterFile = function (f) {
        if (_filetypes.isArchiveFile(f)) {
            return (_settings.searchArchives && self.isArchiveSearchFile(f));
        }
        return (!_settings.archivesOnly && self.isSearchFile(f));
    };

    var getSearchFilesForDirectory = function (dir) {
        var searchFiles = [];
        var dirFiles = getFilesForDirectory(dir);
        for (var i=0; i < dirFiles.length; i++) {
            var f = dirFiles[i];
            if (self.filterFile(f)) {
                searchFiles.push(f);
            }
        }
        return searchFiles;
    };

    var getSearchFiles = function (searchDirs) {
        var searchFiles = [];
        var stats = fs.statSync(_settings.startPath);
        if (stats.isDirectory()) {
            for (var i=0; i < searchDirs.length; i++) {
                searchFiles.push.apply(searchFiles, getSearchFilesForDirectory(searchDirs[i]));
            }
        } else if (stats.isFile()) {
            searchFiles.push(_settings.startPath);
        }
        return searchFiles;
    };

    self.search = function () {
        if (_settings.verbose)
            common.log("Search initiated");

        // initialize reusable counter
        var i = 0;
        // get the search dirs
        var dirs = [];
        dirs.push.apply(dirs, getSearchDirs(_settings.startPath));
        if (_settings.verbose) {
            common.log("\nDirectories to be searched ({0}):".format(dirs.length));
            for (i=0; i < dirs.length; i++) {
                common.log(dirs[i]);
            }
        }

        // get the search files
        var files = getSearchFiles(dirs);
        if (_settings.verbose) {
            common.log("\nFiles to be searched ({0}):".format(files.length));
            for (i=0; i < files.length; i++) {
                common.log(files[i]);
            }
            common.log("");
        }

        for (i=0; i < files.length; i++) {
            searchFile(files[i]);
        }

        if (_settings.verbose)
            common.log("Search complete.");
    };

    var searchFile = function (filepath) {
        var filetype = _filetypes.getFileType(filepath);
        if (filetype === FileType.TEXT) {
            searchTextFile(filepath);
        } else if (filetype === FileType.BINARY) {
            searchBinaryFile(filepath);
        }
    };

    var searchBinaryFile = function (filepath) {
        if (_settings.verbose) {
            common.log('Searching binary file: "{0}"'.format(filepath));
        }
        var contents = FileUtil.getFileContents(filepath);
        var pattern = '';
        var patternResults = {};
        for (var i=0; i < _settings.searchPatterns.length; i++) {
            pattern = new RegExp(_settings.searchPatterns[i].source, "g");
            if (_settings.firstMatch && (pattern.source in patternResults)) {
                break;
            }
            var match = pattern.exec(contents);
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
        }
    };

    var searchTextFile = function (filepath) {
        if (_settings.verbose) {
            common.log('Searching text file {0}'.format(filepath));
        }
        if (_settings.multilineSearch) {
            searchTextFileContents(filepath);
        } else {
            searchTextFileLines(filepath);
        }
    };

    var getLineCount = function (contents) {
        var lineCount = 0;
        if (contents) {
            var matches = contents.match(/(\r?\n)/g);
            if (matches) lineCount = matches.length;
        }
        return lineCount;
    };

    var searchTextFileContents = function (filepath) {
        var contents = FileUtil.getFileContents(filepath);
        var results = self.searchMultiLineString(contents);
        for (var i=0; i < results.length; i++) {
            var r = results[i];
            var resultWithFilepath =
                new SearchResult(r.pattern, filepath, r.linenum,
                        r.matchStartIndex, r.matchEndIndex, r.line,
                        r.linesBefore, r.linesAfter);
            addSearchResult(resultWithFilepath);
        }
    };

    var getNewLineIndices = function (s) {
        var indices = [];
        for (var i = 0; i < s.length; i++) {
            if (s.charAt(i) == "\n") {
                indices.push(i);
            }
        }
        return indices;
    };

    var getLinesAtIndices = function(s, atIndices, startLineIndices, endLineIndices) {
        if (atIndices.length === 0)
            return [];
        var lines = [];
        for (var i=0; i < atIndices.length; i++) {
            var a = atIndices[i];
            var line = s.substring(a, endLineIndices[startLineIndices.indexOf(a)]);
            lines.push(line);
        }
        return lines;
    };

    var getLinesBefore = function(s, beforeStartIndices, startLineIndices, endLineIndices) {
        return getLinesAtIndices(s, beforeStartIndices, startLineIndices, endLineIndices);
    };

    var getLinesAfter = function(s, afterStartIndices, startLineIndices, endLineIndices) {
        return getLinesAtIndices(s, afterStartIndices, startLineIndices, endLineIndices);
    };

    var getLessThanOrEqual = function(matchVal) {
        return function(i) { return i <= matchVal; };
    };

    var getGreaterThan = function(matchVal) {
        return function(i) { return i > matchVal; };
    };

    self.searchMultiLineString = function (s) {
        var patternResults = {};
        var linesBefore = [];
        var linesAfter = [];
        var results = [];
        var newLineIndices = getNewLineIndices(s);
        var plusOne = function(i) { return i+1; };
        var startLineIndices = [0].concat(newLineIndices.map(plusOne));
        var endLineIndices = newLineIndices.concat([s.length - 1]);
        for (var i=0; i < _settings.searchPatterns.length; i++) {
            var pattern = new RegExp(_settings.searchPatterns[i].source, "g");
            var match = pattern.exec(s);
            var stop = false;
            while (match && !stop) {
                if (_settings.firstMatch && pattern.source in patternResults) {
                    stop = true;
                    continue;
                }
                var lessOrEqual = getLessThanOrEqual(match.index);
                var greaterThan = getGreaterThan(match.index);
                var lineStartIndex = 0;
                var lineEndIndex = s.length - 1;
                var beforeLineCount = 0;
                var beforeStartIndices = startLineIndices.filter(lessOrEqual);
                if (beforeStartIndices.length > 0) {
                    lineStartIndex = beforeStartIndices.pop();
                    beforeLineCount = beforeStartIndices.length;
                    if (beforeStartIndices.length > _settings.linesBefore) {
                        beforeStartIndices = beforeStartIndices.slice(
                            beforeStartIndices.length - _settings.linesBefore);
                    }
                }
                lineEndIndex = endLineIndices[startLineIndices.indexOf(lineStartIndex)];
                var line = s.substring(lineStartIndex, lineEndIndex);
                if (_settings.linesBefore && beforeLineCount) {
                    linesBefore = getLinesBefore(s, beforeStartIndices,
                        startLineIndices, endLineIndices);
                }
                if (_settings.linesAfter) {
                    var afterStartIndices = startLineIndices.filter(greaterThan);
                    if (afterStartIndices.length > _settings.linesAfter) {
                        afterStartIndices = afterStartIndices.slice(0,
                            _settings.linesAfter);
                    }
                    linesAfter = getLinesAfter(s, afterStartIndices,
                        startLineIndices, endLineIndices);
                }
                var matchStartIndex = match.index - lineStartIndex + 1;
                var matchEndIndex = pattern.lastIndex - lineStartIndex + 1;
                if ((_settings.linesBefore === 0 || linesBeforeMatch(linesBefore)) &&
                    (_settings.linesAfter === 0 || linesAfterMatch(linesAfter))) {
                    var searchResult = new SearchResult(
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
        }
        return results;
    };

    var linesMatch = function(lines, inPatterns, outPatterns) {
        return ((inPatterns.length === 0 || anyMatchesAnyPattern(lines, inPatterns)) &&
               (outPatterns.length === 0 || ! anyMatchesAnyPattern(lines, outPatterns)));
    };

    var linesBeforeMatch = function(linesBefore) {
        return linesMatch(linesBefore, _settings.inLinesBeforePatterns,
            _settings.outLinesBeforePatterns);
    };

    var linesAfterMatch = function(linesAfter) {
        return linesMatch(linesAfter, _settings.inLinesAfterPatterns,
            _settings.outLinesAfterPatterns);
    };

    var searchTextFileLines = function (filepath) {
        var lines = FileUtil.getFileLines(filepath);
        var results = self.searchLines(lines);
        for (var i=0; i < results.length; i++) {
            var r = results[i];
            var resultWithFilepath =
                new SearchResult(r.pattern, filepath, r.linenum,
                        r.matchStartIndex, r.matchEndIndex, r.line,
                        r.linesBefore, r.linesAfter);
            addSearchResult(resultWithFilepath);
        }
    };

    // return results so that filepath can be added to them
    self.searchLines = function (lines) {
        var linenum = 0;
        var pattern;
        var linesBefore = [];
        var linesAfter = [];
        var results = [];
        var patternResults = {};
        while (true) {
            if (Object.keys(patternResults).length === _settings.searchPatterns.length) {
                break;
            }
            var line = "";
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
            for (var i=0; i < _settings.searchPatterns.length; i++) {
                pattern = new RegExp(_settings.searchPatterns[i].source, "g");
                var match = pattern.exec(line);
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
            }
            if (_settings.linesBefore > 0) {
                if (linesBefore.length == _settings.linesBefore)
                    linesBefore.shift();
                if (linesBefore.length < _settings.linesBefore)
                    linesBefore.push(line);
            }
        }
        return results;
    };

    var addSearchResult = function (result) {
        self.results.push(result);
    };

    function cmpSearchResults(r1, r2) {
        var pathCmp = path.dirname(r1.filename).localeCompare(path.dirname(r2.filename));
        if (pathCmp === 0) {
            var fileCmp = path.basename(r1.filename).localeCompare(path.basename(r2.filename));
            if (fileCmp === 0) {
                if (r1.linenum === r2.linenum) {
                    return r1.matchStartIndex - r2.matchStartIndex;
                }
                return r1.linenum - r2.linenum;
            }
            return fileCmp;
        }
        return pathCmp;
    }

    self.printSearchResults = function () {
        // first sort the results
        self.results.sort(cmpSearchResults);
        common.log("\nSearch results ({0}):".format(self.results.length));
        for (var i=0; i < self.results.length; i++) {
            common.log(self.results[i].toString());
        }
    };

    self.getMatchingDirs = function () {
        var dirs = [];
        for (var i=0; i < self.results.length; i++) {
            var result = self.results[i];
            dirs.push(path.dirname(result.filename));
        }
        return common.setFromArray(dirs);
    };

    self.printMatchingDirs = function () {
        var dirs = self.getMatchingDirs();
        common.logArray("\nDirectories with matches ({0}):".format(dirs.length), dirs);
    };

    self.getMatchingFiles = function () {
        var files = [];
        for (var i=0; i < self.results.length; i++) {
            var result = self.results[i];
            files.push(result.filename);
        }
        return common.setFromArray(files);
    };

    self.printMatchingFiles = function () {
        var files = self.getMatchingFiles();
        common.logArray("\nFiles with matches ({0}):".format(files.length), files);
    };

    self.getMatchingLines = function () {
        var lines = [];
        for (var i=0; i < self.results.length; i++) {
            var result = self.results[i];
            if (result.linenum)
                lines.push(result.line.trim());
        }
        if (_settings.uniqueLines)
            lines = common.setFromArray(lines);
        lines.sort(function (a, b) {
            if (a.toUpperCase() == b.toUpperCase())
                return 0;
            return a.toUpperCase() < b.toUpperCase() ? -1 : 1;
        });
        return lines;
    };

    self.printMatchingLines = function () {
        var lines = self.getMatchingLines();
        var hdrText;
        if (_settings.uniqueLines)
            hdrText = "\nUnique lines with matches ({0}):";
        else
            hdrText = "\nLines with matches ({0}):";
        common.log(hdrText.format(lines.length));
        for (var i=0; i < lines.length; i++) {
            common.log(lines[i]);
        }
    };
}

exports.Searcher = Searcher;