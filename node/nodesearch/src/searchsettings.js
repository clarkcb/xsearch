/*
 * searchsettings.js
 *
 * represents the settings to use when performing the search
 */

function SearchSettings() {
    var self = this;
    self.startPath = "";
    self.inExtensions = [];
    self.outExtensions = [];
    self.inDirPatterns = [];
    self.outDirPatterns = [];
    self.inFilePatterns = [];
    self.outFilePatterns = [];
    self.inArchiveExtensions = [];
    self.outArchiveExtensions = [];
    self.inArchiveFilePatterns = [];
    self.outArchiveFilePatterns = [];
    self.inLinesAfterPatterns = [];
    self.outLinesAfterPatterns = [];
    self.inLinesBeforePatterns = [];
    self.outLinesBeforePatterns = [];
    self.linesAfterToPatterns = [];
    self.linesAfterUntilPatterns = [];
    self.searchPatterns = [];
    self.archivesOnly = false;
    self.debug = false;
    self.doTiming = false;
    self.excludeHidden = true;
    self.firstMatch = false;
    self.linesAfter = 0;
    self.linesBefore = 0;
    self.listDirs = false;
    self.listFiles = false;
    self.listLines = false;
    self.maxLineLength = 150;
    self.multilineSearch = false;
    self.printResults = false;
    self.printUsage = false;
    self.printVersion = false;
    self.recursive = true;
    self.searchArchives = false;
    self.uniqueLines = false;
    self.verbose = false;
    var addExtensions = function (exts, arr) {
        var xs = exts.split(/,/);
        for (var i in xs) {
            if (xs[i] !== '')
                arr.push(xs[i]);
        }
    };
    self.addInExtension = function (ext) {
        addExtensions(ext, self.inExtensions);
    };
    self.addOutExtension = function (ext) {
        addExtensions(ext, self.outExtensions);
    };
    var addPattern = function (pattern, arr) {
        arr.push(new RegExp(pattern));
    };
    self.addInDirPattern = function (pattern) {
        addPattern(pattern, self.inDirPatterns);
    };
    self.addOutDirPattern = function (pattern) {
        addPattern(pattern, self.outDirPatterns);
    };
    self.addInFilePattern = function (pattern) {
        addPattern(pattern, self.inFilePatterns);
    };
    self.addOutFilePattern = function (pattern) {
        addPattern(pattern, self.outFilePatterns);
    };
    self.addSearchPattern = function (pattern) {
        addPattern(pattern, self.searchPatterns);
    };
    self.addInArchiveExtension = function (ext) {
        addExtensions(ext, self.inArchiveExtensions);
    };
    self.addOutArchiveExtension = function (ext) {
        addExtensions(ext, self.outArchiveExtensions);
    };
    self.addInArchiveFilePattern = function (pattern) {
        addPattern(pattern, self.inArchiveFilePatterns);
    };
    self.addOutArchiveFilePattern = function (pattern) {
        addPattern(pattern, self.outArchiveFilePatterns);
    };
    self.addInLinesAfterPattern = function (pattern) {
        addPattern(pattern, self.inLinesAfterPatterns);
    };
    self.addOutLinesAfterPattern = function (pattern) {
        addPattern(pattern, self.outLinesAfterPatterns);
    };
    self.addInLinesBeforePattern = function (pattern) {
        addPattern(pattern, self.inLinesBeforePatterns);
    };
    self.addOutLinesBeforePattern = function (pattern) {
        addPattern(pattern, self.outLinesBeforePatterns);
    };
    self.addLinesAfterToPattern = function (pattern) {
        addPattern(pattern, self.linesAfterToPatterns);
    };
    self.addLinesAfterUntilPattern = function (pattern) {
        addPattern(pattern, self.linesAfterUntilPatterns);
    };

    self.setArchivesOnly = function () {
        self.archivesOnly = true;
        self.searchArchives = true;
    };

    self.setDebug = function () {
        self.debug = true;
        self.verbose = true;
    };

    var listToString = function (name, lst) {
        return name + '=["' + lst.join('","') + '"]';
    };

    self.toString = function () {
        var s = 'SearchSettings(';
        s = s + 'archivesOnly=' + self.archivesOnly;
        s = s + ', debug=' + self.debug;
        s = s + ', doTiming=' + self.doTiming;
        s = s + ', excludeHidden=' + self.excludeHidden;
        s = s + ', firstMatch=' + self.firstMatch;
        s = s + ', ' + listToString('inArchiveExtensions', self.inArchiveExtensions);
        s = s + ', ' + listToString('inArchiveFilePatterns', self.inArchiveFilePatterns);
        s = s + ', ' + listToString('inDirPatterns', self.inDirPatterns);
        s = s + ', ' + listToString('inExtensions', self.inExtensions);
        s = s + ', ' + listToString('inFilePatterns', self.inFilePatterns);
        s = s + ', ' + listToString('inLinesAfterPatterns', self.inLinesAfterPatterns);
        s = s + ', ' + listToString('inLinesBeforePatterns', self.inLinesBeforePatterns);
        s = s + ', linesAfter=' + self.linesAfter;
        s = s + ', ' + listToString('linesAfterToPatterns', self.linesAfterToPatterns);
        s = s + ', ' + listToString('linesAfterUntilPatterns', self.linesAfterUntilPatterns);
        s = s + ', linesBefore=' + self.linesBefore;
        s = s + ', listDirs=' + self.listDirs;
        s = s + ', listFiles=' + self.listFiles;
        s = s + ', listLines=' + self.listLines;
        s = s + ', maxLineLength=' + self.maxLineLength;
        s = s + ', multilineSearch=' + self.multilineSearch;
        s = s + ', ' + listToString('outArchiveExtensions', self.outArchiveExtensions);
        s = s + ', ' + listToString('outArchiveFilePatterns', self.outArchiveFilePatterns);
        s = s + ', ' + listToString('outDirPatterns', self.outDirPatterns);
        s = s + ', ' + listToString('outExtensions', self.outExtensions);
        s = s + ', ' + listToString('outFilePatterns', self.outFilePatterns);
        s = s + ', ' + listToString('outLinesAfterPatterns', self.outLinesAfterPatterns);
        s = s + ', ' + listToString('outLinesBeforePatterns', self.outLinesBeforePatterns);
        s = s + ', printResults=' + self.printResults;
        s = s + ', printVersion=' + self.printVersion;
        s = s + ', recursive=' + self.recursive;
        s = s + ', searchArchives=' + self.searchArchives;
        s = s + ', ' + listToString('searchPatterns', self.searchPatterns);
        s = s + ',  startPath="' + self.startPath + '"';
        s = s + ', uniqueLines=' + self.uniqueLines;
        s = s + ', verbose=' + self.verbose;
        s = s + ')';
        return s;
    };
}

exports.SearchSettings = SearchSettings;