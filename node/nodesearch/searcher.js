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
    var self = this;
    var _settings = settings;
    var _filetypes = new FileTypes();
    var _timers = {};
    var _totalElapsed = 0;
    self.results = [];

    var validateSettings = function () {
        assert.ok(_settings.startPath, 'Startpath not defined');
        assert.ok(fs.existsSync(_settings.startPath), 'Startpath not found');
        assert.ok(isSearchDir(_settings.startPath), 'Startpath does not match search settings');
        assert.ok(_settings.searchPatterns.length, 'No search patterns specified');
    };

    var matchesAnyElement = function (s, elements) {
        return elements.indexOf(s) > -1;
    };

    var matchesAnyPattern = function (s, patterns) {
        for (var p in patterns) {
            var pattern = patterns[p];
            var match = pattern.exec(s);
            if (match) {
                return true;
            }
        }
        return false;
    };

    var anyMatchesAnyPattern = function (ss, patterns) {
        for (var i in ss) {
            if (matchesAnyPattern(ss[i], patterns)) {
                return true;
            }
        }
        return false;
    };

    var isSearchDir = function (dir) {
        var pathElems = dir.split(path.sep);
        if (_settings.excludeHidden) {
            for (var p in pathElems) {
                if (!matchesAnyElement(pathElems[p], ['.','..']) &&
                    FileUtil.isHidden(pathElems[p])) {
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

    var isSearchFile = function (file) {
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

    var isArchiveSearchFile = function (file) {
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
        for (var c in childItems) {
            var filepath = path.join(dir, childItems[c]);
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
        for (var d in subDirs) {
            if (isSearchDir(subDirs[d])) {
                searchDirs.push(subDirs[d]);
            }
            searchDirs.push.apply(searchDirs, recGetSearchDirs(subDirs[d]));
        }
        return searchDirs;
    };

    var getFilesForDirectory = function (dir) {
        var files = [];
        var childItems = fs.readdirSync(dir);
        for (var c in childItems) {
            var filepath = path.join(dir, childItems[c]);
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

    var filterFile = function (f) {
        if (_filetypes.isArchiveFile(f) && _settings.searchArchives &&
            isArchiveSearchFile(f))
            return true;
        if (!_settings.archivesOnly && isSearchFile(f))
            return true;
        return false;
    };

    var getSearchFilesForDirectory = function (dir) {
        var searchFiles = [];
        var dirFiles = getFilesForDirectory(dir);
        for (var d in dirFiles) {
            var f = dirFiles[d];
            if (filterFile(f)) {
                searchFiles.push(f);
            }
        }
        return searchFiles;
    };

    var getSearchFiles = function (searchDirs) {
        var searchFiles = [];
        var stats = fs.statSync(_settings.startPath);
        if (stats.isDirectory()) {
            for (var d in searchDirs) {
                searchFiles.push.apply(searchFiles, getSearchFilesForDirectory(searchDirs[d]));
            }
        } else if (stats.isFile()) {
            searchFiles.push(_settings.startPath);
        }
        return searchFiles;
    };

    var addTimer = function (name, action) {
        _timers[name + ":" + action] = new Date();
    };

    var startTimer = function (name) {
        addTimer(name, "start");
    };

    var stopTimer = function (name) {
        addTimer(name, "stop");
        addElapsed(name);
    };

    var addElapsed = function (name) {
        _totalElapsed += getElapsed(name);
    };

    var getElapsed = function (name) {
        var start = _timers[name+":start"];
        var stop = _timers[name+":stop"];
        var elapsed = stop - start;
        return elapsed;
    };

    var printElapsed = function (name) {
        var elapsed = getElapsed(name);
        var msg = "Elapsed time for {0}: {1} ms";
        common.log(msg.format(name, elapsed));
    };

    var printTotalElapsed = function () {
        var msg = "Total elapsed time: {0} ms";
        common.log(msg.format(_totalElapsed));
    };

    self.search = function () {
        if (_settings.verbose)
            common.log("Search initiated");

        // get the search dirs
        if (_settings.doTiming)
            startTimer("GetSearchDirs");
        var dirs = [];
        dirs.push.apply(dirs, getSearchDirs(_settings.startPath));
        if (_settings.doTiming) {
            stopTimer("GetSearchDirs");
            if (_settings.printResults)
                printElapsed("GetSearchDirs");
        }
        if (_settings.verbose) {
            common.log("\nDirectories to be searched ({0}):".format(dirs.length));
            for (var d in dirs) {
                common.log(dirs[d]);
            }
        }

        // get the search files
        if (_settings.doTiming)
            startTimer("GetSearchFiles");
        var files = getSearchFiles(dirs);
        if (_settings.doTiming) {
            stopTimer("GetSearchFiles");
            if (_settings.printResults)
                printElapsed("GetSearchFiles");
        }
        if (_settings.verbose) {
            common.log("\nFiles to be searched ({0}):".format(files.length));
            for (var f in files) {
                common.log(files[f]);
            }
            common.log("");
        }

        if (_settings.doTiming)
            startTimer("SearchFiles");
        for (var i in files) {
            searchFile(files[i]);
        }
        if (_settings.doTiming) {
            stopTimer("SearchFiles");
            if (_settings.printResults) {
                printElapsed("SearchFiles");
                printTotalElapsed();
            }
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
        var contents = fs.readFileSync(filepath).toString();
        var pattern = '';
        for (var p in _settings.searchPatterns) {
            pattern = _settings.searchPatterns[p];
            var match = pattern.exec(contents);
            if (match) {
                addSearchResult(new SearchResult(pattern, filepath, 0, 0, 0,
                    null, [], []));
            }
        }
    };

    var searchTextFile = function (filepath) {
        if (_settings.verbose) {
            common.log('Searching text file {0}'.format(filepath));
        }
        if (_settings.multilineSearch)
            searchTextFileContents(filepath);
        else
            searchTextFileLines(filepath);
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
        var contents = fs.readFileSync(filepath).toString();
        var results = self.searchMultiLineString(contents);
        for (var i in results) {
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
        for (var a in atIndices) {
            var i = atIndices[a];
            var line = s.substring(i, endLineIndices[startLineIndices.indexOf(i)]);
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

    var lessOrEqual = function(i) { return i <= match.index; };
    var greaterThan = function(i) { return i > match.index; };

    self.searchMultiLineString = function (s) {
        var patternResults = {};
        var linesBefore = [];
        var linesAfter = [];
        var results = [];
        var newLineIndices = getNewLineIndices(s);
        var plusOne = function(i) { return i+1; };
        var startLineIndices = [0].concat(newLineIndices.map(plusOne));
        var endLineIndices = newLineIndices.concat([s.length - 1]);
        for (var p in _settings.searchPatterns) {
            var pattern = new RegExp(_settings.searchPatterns[p].source, "g");
            if (_settings.firstMatch && pattern in patternResults)
                continue;
            var match = pattern.exec(s);
            while (match) {
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
                line = s.substring(lineStartIndex, lineEndIndex);
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
                    if (!(pattern in patternResults))
                        patternResults[pattern] = [];
                    patternResults[pattern].push(searchResult);
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
        var contents = fs.readFileSync(filepath).toString();
        var lines = contents.toString().split(/\r?\n/);
        var results = searchLines(lines);
        for (var i in results) {
            var r = results[i];
            var resultWithFilepath =
                new SearchResult(r.pattern, filepath, r.linenum,
                        r.matchStartIndex, r.matchEndIndex, r.line,
                        r.linesBefore, r.linesAfter);
            addSearchResult(resultWithFilepath);
        }
    };

    // return results so that filepath can be added to them
    var searchLines = function (lines) {
        var linenum = 0;
        var pattern;
        var linesBefore = [];
        var linesAfter = [];
        var results = [];
        while (true) {
            var line = "";
            if (linesAfter.length > 0) {
                line = linesAfter.shift();
            } else if (lines.length > 0) {
                line = lines.shift();
            } else {
                return results;
            }
            linenum += 1;
            if (_settings.linesAfter > 0) {
                while (linesAfter.length < _settings.linesAfter && lines.length > 0) {
                    linesAfter.push(lines.shift());
                }
            }
            for (var p in _settings.searchPatterns) {
                pattern = new RegExp(_settings.searchPatterns[p].source, "g");
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
    };

    var addSearchResult = function (result) {
        self.results.push(result);
    };

    self.getMatchingDirs = function () {
        var dirs = [];
        for (var r in self.results) {
            var result = self.results[r];
            dirs.push(path.dirname(result.filename));
        }
        return common.setFromArray(dirs);
    };

    self.printMatchingDirs = function () {
        var dirs = self.getMatchingDirs();
        common.log("\nDirectories with matches ({0}):".format(dirs.length));
        for (var d in dirs) {
            common.log(dirs[d]);
        }
    };

    self.getMatchingFiles = function () {
        var files = [];
        for (var r in self.results) {
            var result = self.results[r];
            files.push(result.filename);
        }
        return common.setFromArray(files);
    };

    self.printMatchingFiles = function () {
        var files = self.getMatchingFiles();
        common.log("\nFiles with matches ({0}):".format(files.length));
        for (var f in files) {
            common.log(files[f]);
        }
    };

    self.getMatchingLines = function () {
        var lines = [];
        for (var r in self.results) {
            var result = self.results[r];
            if (result.linenum)
                lines.push(result.line.trim());
        }
        if (_settings.uniqueLines)
            lines = common.setFromArray(lines);
        lines.sort();
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
        for (var l in lines) {
            common.log(lines[l]);
        }
    };
}

exports.Searcher = Searcher;