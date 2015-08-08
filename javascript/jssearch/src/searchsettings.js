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
        for (var i = 0; i < xs.length; i++) {
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
        return 'SearchSettings('
            + 'archivesOnly=' + this.archivesOnly
            + ', debug=' + this.debug
            + ', doTiming=' + this.doTiming
            + ', excludeHidden=' + this.excludeHidden
            + ', firstMatch=' + this.firstMatch
            + ', ' + listToString('inArchiveExtensions', this.inArchiveExtensions)
            + ', ' + listToString('inArchiveFilePatterns', this.inArchiveFilePatterns)
            + ', ' + listToString('inDirPatterns', this.inDirPatterns)
            + ', ' + listToString('inExtensions', this.inExtensions)
            + ', ' + listToString('inFilePatterns', this.inFilePatterns)
            + ', ' + listToString('inLinesAfterPatterns', this.inLinesAfterPatterns)
            + ', ' + listToString('inLinesBeforePatterns', this.inLinesBeforePatterns)
            + ', linesAfter=' + this.linesAfter
            + ', ' + listToString('linesAfterToPatterns', this.linesAfterToPatterns)
            + ', ' + listToString('linesAfterUntilPatterns', this.linesAfterUntilPatterns)
            + ', linesBefore=' + this.linesBefore
            + ', listDirs=' + this.listDirs
            + ', listFiles=' + this.listFiles
            + ', listLines=' + this.listLines
            + ', maxLineLength=' + this.maxLineLength
            + ', multilineSearch=' + this.multilineSearch
            + ', ' + listToString('outArchiveExtensions', this.outArchiveExtensions)
            + ', ' + listToString('outArchiveFilePatterns', this.outArchiveFilePatterns)
            + ', ' + listToString('outDirPatterns', this.outDirPatterns)
            + ', ' + listToString('outExtensions', this.outExtensions)
            + ', ' + listToString('outFilePatterns', this.outFilePatterns)
            + ', ' + listToString('outLinesAfterPatterns', this.outLinesAfterPatterns)
            + ', ' + listToString('outLinesBeforePatterns', this.outLinesBeforePatterns)
            + ', printResults=' + this.printResults
            + ', printVersion=' + this.printVersion
            + ', recursive=' + this.recursive
            + ', searchArchives=' + this.searchArchives
            + ', ' + listToString('searchPatterns', this.searchPatterns)
            + ',  startPath="' + this.startPath + '"'
            + ', uniqueLines=' + this.uniqueLines
            + ', verbose=' + this.verbose
            + ')';
    };
}

exports.SearchSettings = SearchSettings;