/*
 * nodesearch.js
 *
 * file search utility written in node.js
 */

var fs = require('fs');
var path = require('path');

var fileutil = require('./fileutil.js');
var FileUtil = fileutil.FileUtil;
var searchresult = require('./searchresult.js');
var SearchResult = searchresult.SearchResult;

function Searcher(settings) {
    var that = this;
    if (!settings || !settings.startPath || !settings.searchPatterns.length) {
        throw new Error("Searcher instantiated with invalid SearchSettings instance");
    }
    var _settings = settings;
    var _fileutil = new FileUtil();
    var _timers = {};
    this.results = [];

    var matchesAnyElement = function (s, elements) {
        return elements.indexOf(s) > -1;
    };

    var matchesAnyPattern = function (s, patterns) {
        for (var p in patterns) {
            pattern = patterns[p];
            var match = pattern.exec(s);
            if (match) {
                return true;
            }
        }
        return false;
    };

    var _fileFilterPredicates = (function () {
        preds = [];
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
        for (p in _fileFilterPredicates) {
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
                console.log('childItem: "'+filepath+'"');
            }
            try {
                var stats = fs.statSync(filepath);
                if (stats.isDirectory()) {
                    if (_settings.debug) {
                        console.log('"'+filepath+'" is a directory')
                    }
                    dirs.push(filepath);
                } else if (stats.isFile()) {
                    if (_settings.debug) {
                        console.log('"'+filepath+'" is a file')
                    }
                    if (isTargetFile(filepath)) {
                        files.push(filepath);
                    }
                } else {
                    if (_settings.verbose || _settings.debug) {
                        console.log("childItem neither directory nor file: "+filepath);
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
    }

    var startTimer = function (name) {
        var start = new Date();
        _timers[name+":start"] = start;
    }

    var stopTimer = function (name) {
        var start = _timers[name+":start"];
        var stop = new Date();
        _timers[name+":stop"] = stop;
        var elapsed = stop - start;
        _timers[name+":elapsed"] = elapsed;
        console.log("Elapsed time for " + name + ": " + elapsed + " milliseconds");
    }

    this.search = function () {
        if (_settings.verbose || _settings.debug) {
            console.log("Search initiated");
        }
        if (!fs.existsSync(_settings.startPath)) {
            throw new Error('Path not found: '+_settings.startPath);
        }
        var stats = fs.statSync(_settings.startPath);
        if (!stats.isDirectory()) {
            throw new Error("searchSettings.startPath is not a valid directory: "+_settings.startPath);
        }

        if (_settings.doTiming) {
            startTimer("GetSearchFiles");
        }
        var files = getSearchFiles(_settings.startPath);
        if (_settings.doTiming) {
            stopTimer("GetSearchFiles");
        }

        if (_settings.doTiming) {
            startTimer("SearchFiles");
        }
        for (i in files) {
            searchFile(files[i]);
        }
        if (_settings.doTiming) {
            stopTimer("SearchFiles");
        }
        if (_settings.verbose) {
            console.log("Search completed");
        }
        if (_settings.doTiming && _settings.debug) {
            console.log("\nTimers:");
            for (var t in _timers) {
                console.log(t + ": " + _timers[t]);
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
        var contents = fs.readFileSync(filepath).toString();
        var lines = contents.toString().split(/(\r\n|\n)/);
        var linenum = 0;
        var pattern = '';
        for (i in lines) {
            linenum += 1;
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
    }

    var setFromArray = function (arr) {
        hash = {};
        set = [];
        for (var a in arr) {
            hash[arr[a]] = 1;
        }
        for (var h in hash) {
            set.push(h);
        }
        return set;
    }

    this.getFilesWithMatches = function () {
        var files = [];
        for (var r in that.results) {
            var result = that.results[r];
            files.push(result.filename);
        }
        return setFromArray(files);
    }
}

exports.Searcher = Searcher;