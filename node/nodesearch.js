/*
 * nodesearch.js
 *
 * file search utility written in node.js
 */
var fs = require('fs');
var path = require('path');

function FileUtil() {
    var that = this;
    var NOSEARCH_EXTS   = 'aif aifc aiff au avi bmp cab dmg eps gif ' +
                          'ico idlk ief iso jpe jpeg jpg ' +
                          'm3u m4a m4p mov movie mp3 mp4 mpe mpeg mpg mxu ' +
                          'ogg pdf pict png ps qt ra ram rm rpm ' +
                          'scc snd suo tif tiff wav'.split(/\s+/);
    var BINARY_EXTS     = 'ai bin class com dat dbmdl dcr dir dll dxr dms doc docx dot exe ' +
                          'hlp indd lnk mo obj pdb ppt psd pyc pyo qxd so swf sys ' +
                          'vsd xls xlsx xlt'.split(/\s+/);
    var COMPRESSED_EXTS = 'bz2 cpio ear gz hqx jar pax rar sit sitx tar tgz ' +
                         'war zip Z'.split(/\s+/);
    var TEXT_EXTS       = '1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ' +
                          'am app as asc ascx asm asp aspx bash bat bdsproj bsh ' +
                          'c cc cfg clj cls cmd cnt conf config cpp cs csh csproj css csv ctl ' +
                          'dat dbproj dbml dbschema ddl dep dfm disco dlg dof dpr dsp dsw dtd ' +
                          'env etx exp fls fs fsproj h hpp htm html ics iml in inc ini ipr iws ' +
                          'java js jsp layout log mak map master mht mxml ' +
                          'pas php php3 pl plist pm po properties py ' +
                          'rb rc rc2 rdf resx rex rtf rtx ' +
                          'scala scc sgm sgml sh sln smi smil spec sqc sql st str ' +
                          'strings suml svg sxw ' +
                          't tcl tld tmx tsv txt url user ' +
                          'vb vbproj vbs vcf vcproj vdproj vm vrml vssscc vxml ' +
                          'wbxml webinfo wml wmls wrl wsd wsdd wsdl ' +
                          'xlf xml xsd xsl xslt'.split(/\s+/);
    var UNKNOWN_EXTS    = 'adm aps cli clw dat db def df2 ncb nt nt2 orig ' +
                          'pc plg roff sun t tex texinfo tr xwd'.split(/\s+/);
    var SEARCHABLE_EXTS = BINARY_EXTS.concat(COMPRESSED_EXTS, TEXT_EXTS);
    this.getExtension = function (filename) {
        var idx = filename.lastIndexOf('.');
        if (idx > 0 && idx < filename.length-1) {
            return filename.substring(idx+1);
        } else {
            return '';
        }
    };
    this.isBinaryFile = function (filename) {
        var ext = that.getExtension(filename);
        return BINARY_EXTS.indexOf(ext) > -1;
    };
    this.isCompressedFile = function (filename) {
        var ext = that.getExtension(filename);
        return COMPRESSED_EXTS.indexOf(ext) > -1;
    };
    this.isSearchableFile = function (filename) {
        var ext = that.getExtension(filename);
        return SEARCHABLE_EXTS.indexOf(ext) > -1;
    };
    this.isTextFile = function (filename) {
        var ext = that.getExtension(filename);
        return TEXT_EXTS.indexOf(ext) > -1;
    };
}

function SearchSettings() {
    var that = this;
    this.startPath = "";
    this.inExtensions = [];
    this.outExtensions = [];
    this.inDirnamePatterns = [];
    this.outDirnamePatterns = [];
    this.inFilenamePatterns = [];
    this.outFilenamePatterns = [];
    this.searchPatterns = [];
    this.debug = false;
    this.doTiming = false;
    this.firstMatch = false;
    this.listFiles = false;
    this.verbose = false;
    var addExtension = function (ext, arr) {
        arr.push(ext);
    };
    this.addInExtension = function (ext) {
        addExtension(ext, that.inExtensions);
    };
    this.addOutExtension = function (ext) {
        addExtension(ext, that.outExtensions);
    };
    var addPattern = function (pattern, arr) {
        arr.push(new RegExp(pattern));
    };
    this.addInDirnamePattern = function (pattern) {
        addPattern(pattern, that.inDirnamePatterns);
    };
    this.addOutDirnamePattern = function (pattern) {
        addPattern(pattern, that.outDirnamePatterns);
    };
    this.addInFilenamePattern = function (pattern) {
        addPattern(pattern, that.inFilenamePatterns);
    };
    this.addOutFilenamePattern = function (pattern) {
        addPattern(pattern, that.outFilenamePatterns);
    };
    this.addSearchPattern = function (pattern) {
        addPattern(pattern, that.searchPatterns);
    };
    this.toString = function () {
        var s = 'SearchSettings(startPath="' + that.startPath + '"';
        if (that.inExtensions.length) {
            s = s + ', inExtensions=["' + that.inExtensions.join('","') + '"]';
        }
        if (that.outExtensions.length) {
            s = s + ', outExtensions=["' + that.outExtensions.join('","') + '"]';
        }
        if (that.inFilenamePatterns.length) {
            s = s + ', inFilenamePatterns=["' + that.inFilenamePatterns.join('","') + '"]';
        }
        if (that.outFilenamePatterns.length) {
            s = s + ', outFilenamePatterns=["' + that.outFilenamePatterns.join('","') + '"]';
        }
        if (that.searchPatterns.length) {
            s = s + ', searchPatterns=["' + that.searchPatterns.join('","') + '"]';
        }
        s = s + ', doTiming=' + that.doTiming + ', verbose=' + that.verbose + ')';
        return s;
    };
};

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
        if (!path.existsSync(_settings.startPath)) {
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

function SearchResult(pattern, filename, linenum, line) {
    var that = this;
    this.pattern = pattern;
    this.filename = filename;
    this.linenum = linenum;
    this.line = line;

    this.toString = function () {
        var s = that.filename;
        if (that.linenum) {
            s = s + ': ' + that.linenum + ': ' + that.line.trim();
        } else {
            s = s + ' has match';
        }
        return s;
    };
}

function searchSettingsFromArgs(args) {
    var settings = new SearchSettings();
    //var argInfo = {
    //    'd': { f: settings.addInDirnamePattern },
    //    'D': { f: settings.addOutDirnamePattern },
    //    'f': { f: settings.addInFilenamePattern },
    //    'F': { f: settings.addOutFilenamePattern },
    //    's': { f: settings.addSearchPattern },
    //    'x': { f: settings.addInExtension },
    //    'X': { f: settings.addOutExtension }
    //};
    var argMap = {
        'd': settings.addInDirnamePattern,
        'D': settings.addOutDirnamePattern,
        'f': settings.addInFilenamePattern,
        'F': settings.addOutFilenamePattern,
        's': settings.addSearchPattern,
        'x': settings.addInExtension,
        'X': settings.addOutExtension
    };
    var flagMap = {
        'debug': function () { settings.debug = true; },
        'listfiles': function () { settings.listfiles = true; },
        't': function () { settings.doTiming = true; },
        'v': function () { settings.verbose = true; },
        '1': function () { settings.firstMatch = true; }
    };
    while(args) {
        var arg = args.shift();
        if (!arg) {
            break;
        }
        if (arg.charAt(0) === '-') {
            while (arg && arg.charAt(0) === '-') {
                arg = arg.substring(1);
            }
            if (argMap[arg]) {
                if (args) {
                    argMap[arg](args.shift());
                } else {
                    throw new Error("Missing argument for option "+arg);
                }
            } else if (flagMap[arg]) {
                flagMap[arg]();
            } else {
                throw new Error("Unknown option: "+arg);
            }
        } else {
            settings.startPath = arg;
        }
    }
    return settings;
}

function searchMain() {
    if (process.argv.length < 3) {
        throw new Error("Missing arguments");
    }
    var args = process.argv.slice(2);
    //console.log('args: ', args);
    settings = searchSettingsFromArgs(args);
    if (!settings.startPath) {
        throw new Error("Missing startpath");
    }
    if (!settings.searchPatterns.length) {
        throw new Error("Search pattern not defined");
    }
    if (settings.debug) {
        console.log("settings: " + settings.toString());
    }
    
    var searcher = new Searcher(settings);
    searcher.search();
    
    console.log("Matches: " + searcher.results.length);
    
    if (settings.listfiles) {
        var files = searcher.getFilesWithMatches();
        console.log("\nMatching files:");
        for (var f in files) {
            console.log(files[f]);
        }
    }
}

// node.js equivalent of python's if __name__ == '__main__'
if (!module.parent) {
    searchMain();
}
