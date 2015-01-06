/*
 * nodesearch.js
 *
 * file search utility written in node.js
 */

var assert = require('assert');
var fs = require('fs');
var path = require('path');

var FileUtil = require('./fileutil.js').FileUtil;
var SearchResult = require('./searchresult.js').SearchResult;

// add a startsWith method to String type
if (typeof String.prototype.startsWith != 'function') {
    String.prototype.startsWith = function (str) {
        return this.slice(0, str.length) == str;
    };
}

// add a format method to String type (use {0} style placeholders)
// from http://stackoverflow.com/questions/610406/javascript-equivalent-to-printf-string-format/4673436#4673436
if (!String.prototype.format) {
  String.prototype.format = function() {
    var args = arguments;
    return this.replace(/{(\d+)}/g, function(match, number) { 
      return typeof args[number] != 'undefined'
        ? args[number]
        : match
      ;
    });
  };
}

function Searcher(settings) {
    var that = this;
    var _settings = settings;
    var _fileutil = new FileUtil();
    var _timers = {};
    var _totalElapsed = 0;
    this.results = [];

    var log = function (message) {
        console.log(message);
    }

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

    var isSearchDir = function (dir) {
        var pathElems = dir.split(path.sep);
        if (_settings.excludeHidden) {
            for (var p in pathElems) {
                if (!matchesAnyElement(pathElems[p], ['.','..']) &&
                    pathElems[p].startsWith('.')) {
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
        if (file.startsWith(".") && _settings.excludeHidden) {
            return false;
        }
        if (_settings.inExtensions.length &&
            !matchesAnyElement(_fileutil.getExtension(file), _settings.inExtensions)) {
            return false;
        }
        if (_settings.outExtensions.length &&
            matchesAnyElement(_fileutil.getExtension(file), _settings.outExtensions)) {
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
        if (file.startsWith(".") && _settings.excludeHidden) {
            return false;
        }
        if (_settings.inArchiveExtensions.length &&
            !matchesAnyElement(_fileutil.getExtension(file), _settings.inArchiveExtensions)) {
            return false;
        }
        if (_settings.outArchiveExtensions.length &&
            matchesAnyElement(_fileutil.getExtension(file), _settings.outArchiveExtensions)) {
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
                log("Getting list of directories to search under " + startPath);
            }
            searchDirs.push(startPath)
            if (_settings.recursive) {
                searchDirs.push.apply(searchDirs, recGetSearchDirs(startPath));
            }
        } else if (stats.isFile()) {
            var d = path.dirname(startPath)
            if (!d) d = "."
                searchDirs.push(d)
        }
        return searchDirs;
    }

    var handleFsError = function (err) {
        if (err.errno === 34 && err.code === "ENOENT") {
            // this error seems to occur when the file is a soft link
            // to a non-existent file
        } else {
            log(err);
            process.exit(1);
        }
    }

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
        }
        for (var d in subDirs) {
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
    }

    var getSearchFilesForDirectory = function (dir) {
        var searchFiles = [];
        var dirFiles = getFilesForDirectory(dir);
        for (var d in dirFiles) {
            var f = dirFiles[d];
            if (_fileutil.isArchiveFile(f) && _settings.searchArchives &&
                isArchiveSearchFile(f)) {
                searchFiles.push(f);
            } else if (!_settings.archivesOnly && isSearchFile(f)) {
                searchFiles.push(f);
            }
        }
        return searchFiles;
    }

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
        log(msg.format(name, elapsed));
    };

    var printTotalElapsed = function () {
        var msg = "Total elapsed time: {0} ms";
        log(msg.format(_totalElapsed));
    };

    this.search = function () {
        if (_settings.verbose)
            log("Search initiated");

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
            log("\nDirectories to be searched (" + dirs.length + "):");
            for (var d in dirs) {
                log(dirs[d]);
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
            log("\nFiles to be searched (" + files.length + "):");
            for (var f in files) {
                log(files[f]);
            }
            log("");
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
            log("Search complete.");
    };

    var searchFile = function (filepath) {
        if (_fileutil.isTextFile(filepath)) {
            searchTextFile(filepath);
        } else if (_fileutil.isBinaryFile(filepath)) {
            searchBinaryFile(filepath);
        }
    };

    var searchBinaryFile = function (filepath) {
        if (_settings.verbose) {
            log('Searching binary file: "'+filepath+'"');
        }
        var contents = fs.readFileSync(filepath).toString();
        var pattern = '';
        for (p in _settings.searchPatterns) {
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
            log('Searching text file: "'+filepath+'"');
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
        var results = searchContents(contents);
        for (i in results) {
            var r = results[i];
            var resultWithFilepath =
                new SearchResult(r.pattern, filepath, r.linenum,
                        r.matchStartIndex, r.matchEndIndex, r.line,
                        r.linesBefore, r.linesAfter);
            addSearchResult(resultWithFilepath);
        }
    }

    var searchContents = function (contents) {
        var patternResults = {};
        var results = [];
        for (p in _settings.searchPatterns) {
            var pattern = new RegExp(_settings.searchPatterns[p].source, "g");
            if (_settings.firstMatch && pattern in patternResults)
                continue;
            var match = pattern.exec(contents);
            while (match) {
                var beforeLineCount = 0;
                if (match.index) {
                    var beforeContents = contents.substring(0, match.index);
                    beforeLineCount = getLineCount(beforeContents);
                }
                var afterLineCount = 0;
                if (pattern.lastIndex < contents.length) {
                    var afterContents = contents.substring(pattern.lastIndex);
                    afterLineCount = getLineCount(afterContents);
                }
                var lineStartIndex = match.index;
                while (lineStartIndex > 0 &&
                       contents.charAt(lineStartIndex) != '\n')
                    lineStartIndex -= 1;
                var lineEndIndex = pattern.lastIndex;
                while (lineEndIndex < contents.length &&
                       contents.charAt(lineEndIndex) != '\n')
                    lineEndIndex += 1;
                line = contents.substring(lineStartIndex, lineEndIndex);
                var searchResult = new SearchResult(pattern, '',
                    beforeLineCount+1, 0, 0, line, [], []);
                results.push(searchResult);
                if (!(pattern in patternResults))
                    patternResults[pattern] = [];
                patternResults[pattern].push(searchResult);
                match = pattern.exec(contents);
            }
        }
        return results;
    };

    var searchTextFileLines = function (filepath) {
        var contents = fs.readFileSync(filepath).toString();
        var lines = contents.toString().split(/\r?\n/);
        var results = searchLines(lines);
        for (i in results) {
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
        var results = [];
        for (i in lines) {
            linenum += 1;
            for (p in _settings.searchPatterns) {
                pattern = new RegExp(_settings.searchPatterns[p].source, "g");
                var match = pattern.exec(lines[i]);
                while (match) {
                    results.push(new SearchResult(pattern, '', linenum,
                        match.index+1, pattern.lastIndex+1, lines[i], [], []));
                    if (_settings.firstMatch) {
                        return;
                    }
                    match = pattern.exec(lines[i]);
                }
            }
        }
        return results;
    };

    var addSearchResult = function (result) {
        that.results.push(result);
    };

    var boolHashFromArray = function (arr) {
        var hash = {};
        for (var a in arr) {
            hash[arr[a]] = true;
        }
        return hash;
    };

    var setFromArray = function (arr) {
        var hash = boolHashFromArray(arr);
        var set = [];
        for (var h in hash) {
            set.push(h);
        }
        return set;
    };

    this.getMatchingDirs = function () {
        var dirs = [];
        for (var r in that.results) {
            var result = that.results[r];
            dirs.push(path.dirname(result.filename));
        }
        return setFromArray(dirs);
    };

    this.printMatchingDirs = function () {
        var dirs = that.getMatchingDirs();
        log("\nDirectories with matches ("+dirs.length+"):");
        for (var d in dirs) {
            log(dirs[d]);
        }
    };

    this.getMatchingFiles = function () {
        var files = [];
        for (var r in that.results) {
            var result = that.results[r];
            files.push(result.filename);
        }
        return setFromArray(files);
    };

    this.printMatchingFiles = function () {
        var files = that.getMatchingFiles();
        log("\nFiles with matches ("+files.length+"):");
        for (var f in files) {
            log(files[f]);
        }
    };

    this.getMatchingLines = function () {
        var lines = [];
        for (var r in that.results) {
            var result = that.results[r];
            if (result.linenum)
                lines.push(result.line.trim());
        }
        if (_settings.uniqueLines)
            lines = setFromArray(lines);
        lines.sort();
        return lines;
    };

    this.printMatchingLines = function () {
        var lines = that.getMatchingLines();
        var hdrText;
        if (_settings.uniqueLines)
            hdrText = "Unique lines with matches";
        else
            hdrText = "Lines with matches";
        log("\n"+hdrText+" ("+lines.length+"):");
        for (var l in lines) {
            log(lines[l]);
        }
    };
}

exports.Searcher = Searcher;