/*
 * searchsettings.js
 *
 * represents the settings to use when performing the search
 */

const {FindSettings, FileTypes, sortByToName, StringUtil} = require('jsfind');

class SearchSettings {
    'use strict'

    #findSettings = null;

    colorize = true;
    firstMatch = false;
    inLinesAfterPatterns = [];
    inLinesBeforePatterns = [];
    linesAfter = 0;
    linesAfterToPatterns = [];
    linesAfterUntilPatterns = [];
    linesBefore = 0;
    listLines = false;
    maxLineLength = 150;
    multilineSearch = false;
    outLinesAfterPatterns = [];
    outLinesBeforePatterns = [];
    printResults = false;
    searchArchives = false;
    searchPatterns = [];
    textFileEncoding = "utf-8";
    uniqueLines = false;

    constructor() {
        this.#findSettings = new FindSettings();
    }

    get findSettings() {
        return this.#findSettings;
    }

    get archivesOnly() {
        return this.#findSettings.archivesOnly;
    }

    set archivesOnly(value) {
        this.#findSettings.archivesOnly = value;
        if (value) this.#findSettings.includeArchives = value;
        if (value) this.searchArchives = value;
    }

    get debug() {
        return this.#findSettings.debug;
    }

    set debug(value) {
        this.#findSettings.debug = value;
    }

    get excludeHidden() {
        return this.#findSettings.excludeHidden;
    }

    set excludeHidden(b) {
        this.#findSettings.excludeHidden = b;
    }

    get inArchiveExtensions() {
        return this.#findSettings.inArchiveExtensions;
    }

    set inArchiveExtensions(value) {
        this.#findSettings.inArchiveExtensions = value;
    }

    addInArchiveExtensions(ext) {
        this.#findSettings.addInArchiveExtensions(ext);
    }

    get inArchiveFilePatterns() {
        return this.#findSettings.inArchiveFilePatterns;
    }

    set inArchiveFilePatterns(value) {
        this.#findSettings.inArchiveFilePatterns = value;
    }

    addInArchiveFilePatterns(pattern) {
        this.#findSettings.addInArchiveFilePatterns(pattern);
    }

    get inDirPatterns() {
        return this.#findSettings.inDirPatterns;
    }

    set inDirPatterns(value) {
        this.#findSettings.inDirPatterns = value;
    }

    addInDirPatterns(pattern) {
        this.#findSettings.addPatterns(pattern, this.#findSettings.inDirPatterns);
    }

    get includeArchives() {
        return this.#findSettings.includeArchives;
    }

    set includeArchives(value) {
        this.#findSettings.includeArchives = value;
    }

    get inExtensions() {
        return this.#findSettings.inExtensions;
    }

    set inExtensions(value) {
        this.#findSettings.inExtensions = value;
    }

    addInExtensions(ext) {
        this.#findSettings.addExtensions(ext, this.#findSettings.inExtensions);
    }

    get inFilePatterns() {
        return this.#findSettings.inFilePatterns;
    }

    set inFilePatterns(value) {
        this.#findSettings.inFilePatterns = value;
    }

    addInFilePatterns(pattern) {
        this.#findSettings.addPatterns(pattern, this.#findSettings.inFilePatterns);
    }

    get inFileTypes() {
        return this.#findSettings.inFileTypes;
    }

    set inFileTypes(value) {
        this.#findSettings.inFileTypes = value;
    }

    addInFileTypes(filetype) {
        this.#findSettings.addInFileTypes(filetype);
    }

    addInLinesAfterPatterns(pattern) {
        this.#findSettings.addPatterns(pattern, this.inLinesAfterPatterns);
    }

    addInLinesBeforePatterns(pattern) {
        this.#findSettings.addPatterns(pattern, this.inLinesBeforePatterns);
    }

    addLinesAfterToPatterns(pattern) {
        this.#findSettings.addPatterns(pattern, this.linesAfterToPatterns);
    }

    addLinesAfterUntilPatterns(pattern) {
        this.#findSettings.addPatterns(pattern, this.linesAfterUntilPatterns);
    }

    get listDirs() {
        return this.#findSettings.listDirs;
    }

    set listDirs(value) {
        this.#findSettings.listDirs = value;
    }

    get listFiles() {
        return this.#findSettings.listFiles;
    }

    set listFiles(value) {
        this.#findSettings.listFiles = value;
    }

    get maxDepth() {
        return this.#findSettings.maxDepth;
    }

    set maxDepth(value) {
        this.#findSettings.maxDepth = value;
    }

    get maxLastMod() {
        return this.#findSettings.maxLastMod;
    }

    set maxLastMod(value) {
        this.#findSettings.maxLastMod = value;
    }

    maxLastModForString(s) {
        this.#findSettings.maxLastMod = StringUtil.getDateForString(s);
    }

    get maxSize() {
        return this.#findSettings.maxSize;
    }

    set maxSize(value) {
        this.#findSettings.maxSize = value;
    }

    get minDepth() {
        return this.#findSettings.minDepth;
    }

    set minDepth(value) {
        this.#findSettings.minDepth = value;
    }

    get minLastMod() {
        return this.#findSettings.minLastMod;
    }

    set minLastMod(value) {
        this.#findSettings.minLastMod = value;
    }

    minLastModFromString(s) {
        this.#findSettings.minLastMod = StringUtil.getDateForString(s);
    }

    get minSize() {
        return this.#findSettings.minSize;
    }

    set minSize(value) {
        this.#findSettings.minSize = value;
    }

    get outArchiveExtensions() {
        return this.#findSettings.outArchiveExtensions;
    }

    set outArchiveExtensions(value) {
        this.#findSettings.outArchiveExtensions = value;
    }

    addOutArchiveExtensions(ext) {
        this.#findSettings.addOutArchiveExtensions(ext);
    }

    get outArchiveFilePatterns() {
        return this.#findSettings.outArchiveFilePatterns;
    }

    set outArchiveFilePatterns(value) {
        this.#findSettings.outArchiveFilePatterns = value;
    }

    addOutArchiveFilePatterns(pattern) {
        this.#findSettings.addOutArchiveFilePatterns(pattern);
    }

    get outExtensions() {
        return this.#findSettings.outExtensions;
    }

    set outExtensions(value) {
        this.#findSettings.outExtensions = value;
    }

    addOutExtensions(ext) {
        this.#findSettings.addExtensions(ext, this.#findSettings.outExtensions);
    }

    get outDirPatterns() {
        return this.#findSettings.outDirPatterns;
    }

    set outDirPatterns(value) {
        this.#findSettings.outDirPatterns = value;
    }

    addOutDirPatterns(pattern) {
        this.#findSettings.addPatterns(pattern, this.#findSettings.outDirPatterns);
    }

    get outFilePatterns() {
        return this.#findSettings.outFilePatterns;
    }

    set outFilePatterns(value) {
        this.#findSettings.outFilePatterns = value;
    }

    addOutFilePatterns(pattern) {
        this.#findSettings.addOutFilePatterns(pattern);
    }

    get outFileTypes() {
        return this.#findSettings.outFileTypes;
    }

    set outFileTypes(value) {
        this.#findSettings.outFileTypes = value;
    }

    addOutFileTypes(filetype) {
        this.#findSettings.addOutFileTypes(filetype);
    }

    addOutLinesAfterPatterns(pattern) {
        this.#findSettings.addPatterns(pattern, this.outLinesAfterPatterns);
    }

    addOutLinesBeforePatterns(pattern) {
        this.#findSettings.addPatterns(pattern, this.outLinesBeforePatterns);
    }

    get paths() {
        return this.#findSettings.paths;
    }

    set paths(value) {
        this.#findSettings.paths = value;
    }

    get printUsage() {
        return this.#findSettings.printUsage;
    }

    set printUsage(value) {
        this.#findSettings.printUsage = value;
    }

    get printVersion() {
        return this.#findSettings.printVersion;
    }

    set printVersion(value) {
        this.#findSettings.printVersion = value;
    }

    get recursive() {
        return this.#findSettings.recursive;
    }

    set recursive(value) {
        this.#findSettings.recursive = value;
    }

    addSearchPatterns(pattern) {
        this.#findSettings.addPatterns(pattern, this.searchPatterns);
    }

    get sortBy() {
        return this.#findSettings.sortBy;
    }

    set sortBy(value) {
        this.#findSettings.sortBy = value;
    }

    get sortCaseInsensitive() {
        return this.#findSettings.sortCaseInsensitive;
    }

    set sortCaseInsensitive(value) {
        this.#findSettings.sortCaseInsensitive = value;
    }

    get sortDescending() {
        return this.#findSettings.sortDescending;
    }

    set sortDescending(value) {
        this.#findSettings.sortDescending = value;
    }

    get verbose() {
        return this.#findSettings.verbose;
    }

    set verbose(value) {
        this.#findSettings.verbose = value;
    }

    toString() {
        return 'SearchSettings(' +
            'archivesOnly=' + this.archivesOnly +
            ', colorize=' + this.colorize +
            ', debug=' + this.debug +
            ', excludeHidden=' + this.excludeHidden +
            ', firstMatch=' + this.firstMatch +
            ', ' + StringUtil.listToString('inArchiveExtensions', this.inArchiveExtensions) +
            ', ' + StringUtil.listToString('inArchiveFilePatterns', this.inArchiveFilePatterns) +
            ', ' + StringUtil.listToString('inDirPatterns', this.inDirPatterns) +
            ', ' + StringUtil.listToString('inExtensions', this.inExtensions) +
            ', ' + StringUtil.listToString('inFilePatterns', this.inFilePatterns) +
            ', ' + FileTypes.fileTypesToString('inFileTypes', this.inFileTypes) +
            ', ' + StringUtil.listToString('inLinesAfterPatterns', this.inLinesAfterPatterns) +
            ', ' + StringUtil.listToString('inLinesBeforePatterns', this.inLinesBeforePatterns) +
            ', linesAfter=' + this.linesAfter +
            ', ' + StringUtil.listToString('linesAfterToPatterns', this.linesAfterToPatterns) +
            ', ' + StringUtil.listToString('linesAfterUntilPatterns', this.linesAfterUntilPatterns) +
            ', linesBefore=' + this.linesBefore +
            ', listDirs=' + this.listDirs +
            ', listFiles=' + this.listFiles +
            ', listLines=' + this.listLines +
            ', maxDepth=' + this.maxDepth +
            ', ' + StringUtil.dateToString('maxLastMod', this.maxLastMod) +
            ', maxLineLength=' + this.maxLineLength +
            ', maxSize=' + this.maxSize +
            ', minDepth=' + this.minDepth +
            ', ' + StringUtil.dateToString('minLastMod', this.minLastMod) +
            ', minSize=' + this.minSize +
            ', multilineSearch=' + this.multilineSearch +
            ', ' + StringUtil.listToString('outArchiveExtensions', this.outArchiveExtensions) +
            ', ' + StringUtil.listToString('outArchiveFilePatterns', this.outArchiveFilePatterns) +
            ', ' + StringUtil.listToString('outDirPatterns', this.outDirPatterns) +
            ', ' + StringUtil.listToString('outExtensions', this.outExtensions) +
            ', ' + StringUtil.listToString('outFilePatterns', this.outFilePatterns) +
            ', ' + FileTypes.fileTypesToString('outFileTypes', this.outFileTypes) +
            ', ' + StringUtil.listToString('outLinesAfterPatterns', this.outLinesAfterPatterns) +
            ', ' + StringUtil.listToString('outLinesBeforePatterns', this.outLinesBeforePatterns) +
            ', ' + StringUtil.listToString('paths', this.paths) +
            ', printResults=' + this.printResults +
            ', printUsage=' + this.printUsage +
            ', printVersion=' + this.printVersion +
            ', recursive=' + this.recursive +
            ', searchArchives=' + this.searchArchives +
            ', ' + StringUtil.listToString('searchPatterns', this.searchPatterns) +
            ', sortBy=' + sortByToName(this.sortBy) +
            ', sortCaseInsensitive=' + this.sortCaseInsensitive +
            ', sortDescending=' + this.sortDescending +
            ', textFileEncoding="' + this.textFileEncoding + '"' +
            ', uniqueLines=' + this.uniqueLines +
            ', verbose=' + this.verbose +
            ')';
    }
}

exports.SearchSettings = SearchSettings;
