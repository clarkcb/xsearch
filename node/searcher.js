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

function Searcher(settings) {
    var that = this;
    var _settings = settings;
    var _fileutil = new FileUtil();
    var _timers = {};
    this.results = [];

    var validateSettings = function () {
        assert.ok(_settings.startPath, 'Startpath not defined');
        assert.ok(fs.existsSync(_settings.startPath), 'Startpath not found');
        assert.ok(_settings.searchPatterns.length, 'No search patterns specified');
    };
    validateSettings();

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

    var _fileFilterPredicates = (function () {
        var preds = [];
        if (_settings.inExtensions.length) {
            preds.push(function(f) { return matchesAnyElement(_fileutil.getExtension(f), _settings.inExtensions); });
        }
        if (_settings.outExtensions.length) {
            preds.push(function(f) { return !matchesAnyElement(_fileutil.getExtension(f), _settings.outExtensions); });
        }
        if (_settings.inDirnamePatterns.length) {
            preds.push(function(f) { return matchesAnyPattern(path.dirname(f), _settings.inDirnamePatterns) });
        }
        if (_settings.outDirnamePatterns.length) {
            preds.push(function(f) { return !matchesAnyPattern(path.dirname(f), _settings.outDirnamePatterns) });
        }
        if (_settings.inFilenamePatterns.length) {
            preds.push(function(f) { return matchesAnyPattern(path.basename(f), _settings.inFilenamePatterns) });
        }
        if (_settings.outFilenamePatterns.length) {
            preds.push(function(f) { return !matchesAnyPattern(path.basename(f), _settings.outFilenamePatterns) });
        }
        return preds;
    })();

    if (_settings.debug) {
        for (var p in _fileFilterPredicates) {
            console.log("_fileFilterPredictates["+p+"]: "+_fileFilterPredicates[p]);
        }
    }

    var isTargetFile = function (filepath) {
        for (var p in _fileFilterPredicates) {
            if (!_fileFilterPredicates[p](filepath)) {
                return false;
            }
        }
        return true;
    };

    var getSearchFiles = function (currentPath) {
        if (_settings.verbose || _settings.debug) {
            console.log("Getting list of files to search under " + currentPath);
        }
        var dirs = [];
        var files = [];
        var childItems = fs.readdirSync(currentPath);
        for (var c in childItems) {
            var filepath = path.join(currentPath, childItems[c]);
            if (_settings.debug) {
                console.log('childItem: "' + filepath + '"');
            }
            try {
                var stats = fs.statSync(filepath);
                if (stats.isDirectory()) {
                    dirs.push(filepath);
                } else if (stats.isFile()) {
                    if (isTargetFile(filepath)) {
                        if (_settings.debug) {
                            console.log('"' + filepath + '" is a target file');
                        }
                        files.push(filepath);
                    }
                } else {
                    if (_settings.debug) {
                        console.log("childItem neither directory nor file: " + filepath);
                    }
                }
            } catch (err) {
                if (err.errno === 34 && err.code === "ENOENT") {
                    // this error seems to occur when the file is a soft link to a non-existent file
                    continue;
                } else {
                    console.log(err);
                    process.exit(1);
                }
            }
        }
        for (var d in dirs) {
            files.push.apply(files, getSearchFiles(dirs[d]));
        }
        return files;
    };

    var addTimer = function (name, action) {
        _timers[name + ":" + action] = new Date();
    };

    var startTimer = function (name) {
        addTimer(name, "start");
    };

    var stopTimer = function (name) {
        addTimer(name, "stop");
        if (_settings.printResults)
            printElapsed(name);
    };

    var getElapsed = function (name) {
        var start = _timers[name+":start"];
        var stop = _timers[name+":stop"];
        return stop - start;
    };

    var printElapsed = function (name) {
        var elapsed = getElapsed(name);
        console.log("Elapsed time for " + name + ": " + elapsed + " milliseconds");
    };

    this.search = function () {
        if (_settings.verbose)
            console.log("Search initiated");

        if (_settings.doTiming)
            startTimer("GetSearchFiles");
        var files = getSearchFiles(_settings.startPath);
        if (_settings.doTiming)
            stopTimer("GetSearchFiles");

        if (_settings.doTiming)
            startTimer("SearchFiles");
        for (var i in files) {
            searchFile(files[i]);
        }
        if (_settings.doTiming)
            stopTimer("SearchFiles");

        if (_settings.verbose)
            console.log("Search completed");

        if (_settings.printResults)
            console.log("Matches: " + that.results.length);

        if (_settings.doTiming && _settings.debug) {
            console.log("\nTimers:");
            for (var t in _timers) {
                console.log(t + ": " + _timers[t]);
            }
        }
        if (_settings.listFiles) {
            var files = getFilesWithMatches();
            console.log("\nMatching files:");
            for (var f in files) {
                console.log(files[f]);
            }
        }
        if (_settings.listLines) {
            var files = getLinesWithMatches();
            console.log("\nMatching lines:");
            for (var l in lines) {
                console.log(lines[l]);
            }
        }
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
            console.log('Searching binary file: "'+filepath+'"');
        }
        var contents = fs.readFileSync(filepath).toString();
        var pattern = '';
        for (p in _settings.searchPatterns) {
            pattern = _settings.searchPatterns[p];
            var match = pattern.exec(contents);
            if (match) {
                addSearchResult(new SearchResult(pattern, filepath, 0, null));
            }
        }
    };

    var searchTextFile = function (filepath) {
        if (_settings.verbose) {
            console.log('Searching text file: "'+filepath+'"');
        }
        if (_settings.multilineSearch)
            searchTextFileContents(filepath)
        else
            searchTextFileLines(filepath)
    };

    var getLineCount = function (contents) {
        return contents.match(/(\r?\n)/g).length;
    };

    var searchTextFileContents = function (filepath) {
        if (_settings.verbose) {
            console.log('Searching text file contents: "'+filepath+'"');
        }
        var fileResults = {};
        var contents = fs.readFileSync(filepath).toString();

        for (p in _settings.searchPatterns) {
            var pattern = new RegExp(_settings.searchPatterns[p].source, "g");
            if (_settings.firstMatch && pattern in fileResults)
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
                var searchResult = new SearchResult(pattern,
                    filepath, beforeLineCount+1, line);
                addSearchResult(searchResult);
                if (!(pattern in fileResults))
                    fileResults[pattern] = [];
                fileResults[pattern].push(searchResult);
                match = pattern.exec(contents);
            }
        }
    };

    var searchTextFileLines = function (filepath) {
        if (_settings.verbose) {
            console.log('Searching text file by line: "'+filepath+'"');
        }
        var contents = fs.readFileSync(filepath).toString();
        var lines = contents.toString().split(/\r?\n/);
        var linenum = 0;
        var pattern;
        for (i in lines) {
            linenum += 1;
            //console.log("line["+linenum+"]: "+lines[i]);
            for (p in _settings.searchPatterns) {
                pattern = _settings.searchPatterns[p];
                var match = pattern.exec(lines[i]);
                if (match) {
                    addSearchResult(new SearchResult(pattern, filepath, linenum, lines[i]));
                    if (_settings.firstMatch) {
                        return;
                    }
                }
            }
        }
    };

    var addSearchResult = function (result) {
        console.log(result.toString());
        that.results.push(result);
    };

    var setFromArray = function (arr) {
        var hash = {};
        var set = [];
        for (var a in arr) {
            hash[arr[a]] = 1;
        }
        for (var h in hash) {
            set.push(h);
        }
        return set;
    };

    this.getFilesWithMatches = function () {
        var files = [];
        for (var r in that.results) {
            var result = that.results[r];
            files.push(result.filename);
        }
        return setFromArray(files);
    };

    this.getLinesWithMatches = function () {
        var lines = [];
        for (var r in that.results) {
            var result = that.results[r];
            if (result.linenum)
                lines.push(result.line);
        }
        return setFromArray(lines);
    }
}

exports.Searcher = Searcher;