/*
 * searchsettings.js
 *
 * represents the settings to use when performing the search
 */

function SearchSettings() {
    "use strict";
    let self = this;
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
    const addExtensions = function (exts, arr) {
        exts.split(/,/).filter(x => x !== '').forEach(x => arr.push(x));
    };
    self.addInExtension = function (ext) {
        addExtensions(ext, self.inExtensions);
    };
    self.addOutExtension = function (ext) {
        addExtensions(ext, self.outExtensions);
    };
    const addPattern = function (pattern, arr) {
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

    const listToString = function (name, lst) {
        return `${name}=["${lst.join('","')}"]`;
    };

    self.toString = function () {
        return 'SearchSettings(' +
            'archivesOnly=' + self.archivesOnly +
            ', debug=' + self.debug +
            ', excludeHidden=' + self.excludeHidden +
            ', firstMatch=' + self.firstMatch +
            ', ' + listToString('inArchiveExtensions', self.inArchiveExtensions) +
            ', ' + listToString('inArchiveFilePatterns', self.inArchiveFilePatterns) +
            ', ' + listToString('inDirPatterns', self.inDirPatterns) +
            ', ' + listToString('inExtensions', self.inExtensions) +
            ', ' + listToString('inFilePatterns', self.inFilePatterns) +
            ', ' + listToString('inLinesAfterPatterns', self.inLinesAfterPatterns) +
            ', ' + listToString('inLinesBeforePatterns', self.inLinesBeforePatterns) +
            ', linesAfter=' + self.linesAfter +
            ', ' + listToString('linesAfterToPatterns', self.linesAfterToPatterns) +
            ', ' + listToString('linesAfterUntilPatterns', self.linesAfterUntilPatterns) +
            ', linesBefore=' + self.linesBefore +
            ', listDirs=' + self.listDirs +
            ', listFiles=' + self.listFiles +
            ', listLines=' + self.listLines +
            ', maxLineLength=' + self.maxLineLength +
            ', multilineSearch=' + self.multilineSearch +
            ', ' + listToString('outArchiveExtensions', self.outArchiveExtensions) +
            ', ' + listToString('outArchiveFilePatterns', self.outArchiveFilePatterns) +
            ', ' + listToString('outDirPatterns', self.outDirPatterns) +
            ', ' + listToString('outExtensions', self.outExtensions) +
            ', ' + listToString('outFilePatterns', self.outFilePatterns) +
            ', ' + listToString('outLinesAfterPatterns', self.outLinesAfterPatterns) +
            ', ' + listToString('outLinesBeforePatterns', self.outLinesBeforePatterns) +
            ', printResults=' + self.printResults +
            ', printVersion=' + self.printVersion +
            ', recursive=' + self.recursive +
            ', searchArchives=' + self.searchArchives +
            ', ' + listToString('searchPatterns', self.searchPatterns) +
            ',  startPath="' + self.startPath + '"' +
            ', uniqueLines=' + self.uniqueLines +
            ', verbose=' + self.verbose +
            ')';
    };
}

exports.SearchSettings = SearchSettings;