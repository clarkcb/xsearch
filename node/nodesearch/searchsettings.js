/*
 * searchsettings.js
 *
 * represents the settings to use when performing the search
 */

function SearchSettings() {
    var that = this;
    this.startPath = "";
    this.inExtensions = [];
    this.outExtensions = [];
    this.inDirPatterns = [];
    this.outDirPatterns = [];
    this.inFilePatterns = [];
    this.outFilePatterns = [];
    this.inArchiveExtensions = [];
    this.outArchiveExtensions = [];
    this.inArchiveFilePatterns = [];
    this.outArchiveFilePatterns = [];
    this.inLinesAfterPatterns = [];
    this.outLinesAfterPatterns = [];
    this.inLinesBeforePatterns = [];
    this.outLinesBeforePatterns = [];
    this.linesAfterToPatterns = [];
    this.linesAfterUntilPatterns = [];
    this.searchPatterns = [];
    this.archivesOnly = false;
    this.debug = false;
    this.doTiming = false;
    this.excludeHidden = true;
    this.firstMatch = false;
    this.linesAfter = 0;
    this.linesBefore = 0;
    this.listDirs = false;
    this.listFiles = false;
    this.listLines = false;
    this.maxLineLength = 150;
    this.multilineSearch = false;
    this.printResults = false;
    this.printUsage = false;
    this.printVersion = false;
    this.recursive = true;
    this.searchArchives = false;
    this.uniqueLines = false;
    this.verbose = false;
    var addExtensions = function (exts, arr) {
        var xs = exts.split(/,/);
        for (i in xs) {
            if (xs[i] != "")
                arr.push(xs[i]);
        }
    };
    this.addInExtension = function (ext) {
        addExtensions(ext, that.inExtensions);
    };
    this.addOutExtension = function (ext) {
        addExtensions(ext, that.outExtensions);
    };
    var addPattern = function (pattern, arr) {
        arr.push(new RegExp(pattern));
    };
    this.addInDirPattern = function (pattern) {
        addPattern(pattern, that.inDirPatterns);
    };
    this.addOutDirPattern = function (pattern) {
        addPattern(pattern, that.outDirPatterns);
    };
    this.addInFilePattern = function (pattern) {
        addPattern(pattern, that.inFilePatterns);
    };
    this.addOutFilePattern = function (pattern) {
        addPattern(pattern, that.outFilePatterns);
    };
    this.addSearchPattern = function (pattern) {
        addPattern(pattern, that.searchPatterns);
    };
    this.addInArchiveExtension = function (ext) {
        addExtensions(ext, that.inArchiveExtensions);
    };
    this.addOutArchiveExtension = function (ext) {
        addExtensions(ext, that.outArchiveExtensions);
    };
    this.addInArchiveFilePattern = function (pattern) {
        addPattern(pattern, that.inArchiveFilePatterns);
    };
    this.addOutArchiveFilePattern = function (pattern) {
        addPattern(pattern, that.outArchiveFilePatterns);
    };
    this.addInLinesAfterPattern = function (pattern) {
        addPattern(pattern, that.inLinesAfterPatterns);
    };
    this.addOutLinesAfterPattern = function (pattern) {
        addPattern(pattern, that.outLinesAfterPatterns);
    };
    this.addInLinesBeforePattern = function (pattern) {
        addPattern(pattern, that.inLinesBeforePatterns);
    };
    this.addOutLinesBeforePattern = function (pattern) {
        addPattern(pattern, that.outLinesBeforePatterns);
    };

    this.addLinesAfterToPattern = function (pattern) {
        addPattern(pattern, that.linesAfterToPatterns);
    };
    this.addLinesAfterUntilPattern = function (pattern) {
        addPattern(pattern, that.linesAfterUntilPatterns);
    };

    var listToString = function (name, lst) {
        return name + '=["' + lst.join('","') + '"]';
    } 

    this.toString = function () {
        var s = 'SearchSettings(startPath="' + that.startPath + '"';
        if (that.inExtensions.length) {
            s = s + ', ' + listToString('inExtensions', that.inExtensions);
        }
        if (that.outExtensions.length) {
            s = s + ', ' + listToString('outExtensions', that.outExtensions);
        }
        if (that.inDirPatterns.length) {
            s = s + ', ' + listToString('inDirPatterns', that.inDirPatterns);
        }
        if (that.outDirPatterns.length) {
            s = s + ', ' + listToString('outDirPatterns', that.outDirPatterns);
        }
        if (that.inFilePatterns.length) {
            s = s + ', ' + listToString('inFilePatterns', that.inFilePatterns);
        }
        if (that.outFilePatterns.length) {
            s = s + ', ' + listToString('outFilePatterns', that.outFilePatterns);
        }
        if (that.inArchiveExtensions.length) {
            s = s + ', ' + listToString('inArchiveExtensions', that.inArchiveExtensions);
        }
        if (that.outArchiveExtensions.length) {
            s = s + ', ' + listToString('outArchiveExtensions', that.outArchiveExtensions);
        }
        if (that.inArchiveFilePatterns.length) {
            s = s + ', ' + listToString('inArchiveFilePatterns', that.inArchiveFilePatterns);
        }
        if (that.outArchiveFilePatterns.length) {
            s = s + ', ' + listToString('outArchiveFilePatterns', that.outArchiveFilePatterns);
        }
        if (that.inLinesAfterPatterns.length) {
            s = s + ', ' + listToString('inLinesAfterPatterns', that.inLinesAfterPatterns);
        }
        if (that.outLinesAfterPatterns.length) {
            s = s + ', ' + listToString('outLinesAfterPatterns', that.outLinesAfterPatterns);
        }
        if (that.inLinesBeforePatterns.length) {
            s = s + ', ' + listToString('inLinesBeforePatterns', that.inLinesBeforePatterns);
        }
        if (that.outLinesBeforePatterns.length) {
            s = s + ', ' + listToString('outLinesBeforePatterns', that.outLinesBeforePatterns);
        }
        if (that.linesAfterToPatterns.length) {
            s = s + ', ' + listToString('linesAfterToPatterns', that.linesAfterToPatterns);
        }
        if (that.linesAfterUntilPatterns.length) {
            s = s + ', ' + listToString('linesAfterUntilPatterns', that.linesAfterUntilPatterns);
        }
        if (that.searchPatterns.length) {
            s = s + ', ' + listToString('searchPatterns', that.searchPatterns);
        }
        s = s + ', archivesOnly=' + that.archivesOnly;
        s = s + ', debug=' + that.debug;
        s = s + ', doTiming=' + that.doTiming;
        s = s + ', excludeHidden=' + that.excludeHidden;
        s = s + ', firstMatch=' + that.firstMatch;
        s = s + ', linesAfter=' + that.linesAfter;
        s = s + ', linesBefore=' + that.linesBefore;
        s = s + ', listDirs=' + that.listDirs;
        s = s + ', listFiles=' + that.listFiles;
        s = s + ', listLines=' + that.listLines;
        s = s + ', maxLineLength=' + that.maxLineLength;
        s = s + ', multilineSearch=' + that.multilineSearch;
        s = s + ', printResults=' + that.printResults;
        s = s + ', printVersion=' + that.printVersion;
        s = s + ', recursive=' + that.recursive;
        s = s + ', searchArchives=' + that.searchArchives;
        s = s + ', uniqueLines=' + that.uniqueLines;
        s = s + ', verbose=' + that.verbose;
        s = s + ')';
        return s;
    };
}

exports.SearchSettings = SearchSettings;