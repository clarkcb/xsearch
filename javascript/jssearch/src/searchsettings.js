/*
 * searchsettings.js
 *
 * represents the settings to use when performing the search
 */

const {FileTypes} = require('./filetypes');

class SearchSettings {
    'use strict'

    constructor() {
        this.archivesOnly = false;
        this.colorize = true;
        this.debug = false;
        this.excludeHidden = true;
        this.firstMatch = false;
        this.inArchiveExtensions = [];
        this.inArchiveFilePatterns = [];
        this.inDirPatterns = [];
        this.inExtensions = [];
        this.inFilePatterns = [];
        this.inFileTypes = [];
        this.inLinesAfterPatterns = [];
        this.inLinesBeforePatterns = [];
        this.linesAfter = 0;
        this.linesAfterToPatterns = [];
        this.linesAfterUntilPatterns = [];
        this.linesBefore = 0;
        this.listDirs = false;
        this.listFiles = false;
        this.listLines = false;
        this.maxLineLength = 150;
        this.multilineSearch = false;
        this.outArchiveExtensions = [];
        this.outArchiveFilePatterns = [];
        this.outDirPatterns = [];
        this.outExtensions = [];
        this.outFilePatterns = [];
        this.outFileTypes = [];
        this.outLinesAfterPatterns = [];
        this.outLinesBeforePatterns = [];
        this.printResults = false;
        this.printUsage = false;
        this.printVersion = false;
        this.recursive = true;
        this.paths = [];
        this.searchArchives = false;
        this.searchPatterns = [];
        this.textFileEncoding = "utf-8";
        this.uniqueLines = false;
        this.verbose = false;
    }

    addExtensions(exts, arr) {
        let xs = exts;
        if (typeof(exts) === 'string') {
            xs = exts.split(/,/);
        }
        xs.filter(x => x !== '').forEach(x => arr.push(x));
    }

    addInExtensions(ext) {
        this.addExtensions(ext, this.inExtensions);
    }

    addOutExtensions(ext) {
        this.addExtensions(ext, this.outExtensions);
    }

    addPatterns(patterns, arr) {
        if (typeof(patterns) === 'string') {
            arr.push(new RegExp(patterns));
        } else if (patterns.constructor === Array) {
            patterns.forEach(p => arr.push(new RegExp(p)));
        }
    }

    addInDirPatterns(pattern) {
        this.addPatterns(pattern, this.inDirPatterns);
    }

    addOutDirPatterns(pattern) {
        this.addPatterns(pattern, this.outDirPatterns);
    }

    addInFilePatterns(pattern) {
        this.addPatterns(pattern, this.inFilePatterns);
    }

    addOutFilePatterns(pattern) {
        this.addPatterns(pattern, this.outFilePatterns);
    }

    addSearchPatterns(pattern) {
        this.addPatterns(pattern, this.searchPatterns);
    }

    addInArchiveExtensions(ext) {
        this.addExtensions(ext, this.inArchiveExtensions);
    }

    addOutArchiveExtensions(ext) {
        this.addExtensions(ext, this.outArchiveExtensions);
    }

    addInArchiveFilePatterns(pattern) {
        this.addPatterns(pattern, this.inArchiveFilePatterns);
    }

    addOutArchiveFilePatterns(pattern) {
        this.addPatterns(pattern, this.outArchiveFilePatterns);
    }

    addInLinesAfterPatterns(pattern) {
        this.addPatterns(pattern, this.inLinesAfterPatterns);
    }

    addOutLinesAfterPatterns(pattern) {
        this.addPatterns(pattern, this.outLinesAfterPatterns);
    }

    addInLinesBeforePatterns(pattern) {
        this.addPatterns(pattern, this.inLinesBeforePatterns);
    }

    addOutLinesBeforePatterns(pattern) {
        this.addPatterns(pattern, this.outLinesBeforePatterns);
    }

    addLinesAfterToPatterns(pattern) {
        this.addPatterns(pattern, this.linesAfterToPatterns);
    }

    addLinesAfterUntilPatterns(pattern) {
        this.addPatterns(pattern, this.linesAfterUntilPatterns);
    }

    addFileTypes(filetypes, arr) {
        if (typeof(filetypes) === 'string') {
            filetypes.split(/,/).filter(ft => ft !== '').
            forEach(ft => arr.push(FileTypes.fromName(ft)));
        } else if (filetypes.constructor === Array) {
            filetypes.forEach(ft => arr.push(FileTypes.fromName(ft)));
        }
    }

    addInFileTypes(filetype) {
        this.addFileTypes(filetype, this.inFileTypes);
    }

    addOutFileTypes(filetype) {
        this.addFileTypes(filetype, this.outFileTypes);
    }

    setArchivesOnly(b = true) {
        this.archivesOnly = b;
        if (b) this.searchArchives = b;
    }

    setDebug(b = true) {
        this.debug = b;
        if (b) this.verbose = b;
    }

    listToString(name, lst) {
        if (lst.length) return `${name}=["${lst.join('","')}"]`;
        return `${name}=[]`;
    }

    fileTypesToString(name, fileTypes) {
        if (fileTypes.length) {
            var s = `${name}=[`;
            for (var i=0; i < fileTypes.length; i++) {
                if (i > 0) s += ', ';
                s += '"' + FileTypes.toName(fileTypes[i]) + '"';
            }
            s += ']';
            return s;
        }
        return `${name}=[]`;
    }

    toString() {
        return 'SearchSettings(' +
            'archivesOnly=' + this.archivesOnly +
            ', colorize=' + this.colorize +
            ', debug=' + this.debug +
            ', excludeHidden=' + this.excludeHidden +
            ', firstMatch=' + this.firstMatch +
            ', ' + this.listToString('inArchiveExtensions', this.inArchiveExtensions) +
            ', ' + this.listToString('inArchiveFilePatterns', this.inArchiveFilePatterns) +
            ', ' + this.listToString('inDirPatterns', this.inDirPatterns) +
            ', ' + this.listToString('inExtensions', this.inExtensions) +
            ', ' + this.listToString('inFilePatterns', this.inFilePatterns) +
            ', ' + this.fileTypesToString('inFileTypes', this.inFileTypes) +
            ', ' + this.listToString('inLinesAfterPatterns', this.inLinesAfterPatterns) +
            ', ' + this.listToString('inLinesBeforePatterns', this.inLinesBeforePatterns) +
            ', linesAfter=' + this.linesAfter +
            ', ' + this.listToString('linesAfterToPatterns', this.linesAfterToPatterns) +
            ', ' + this.listToString('linesAfterUntilPatterns', this.linesAfterUntilPatterns) +
            ', linesBefore=' + this.linesBefore +
            ', listDirs=' + this.listDirs +
            ', listFiles=' + this.listFiles +
            ', listLines=' + this.listLines +
            ', maxLineLength=' + this.maxLineLength +
            ', multilineSearch=' + this.multilineSearch +
            ', ' + this.listToString('outArchiveExtensions', this.outArchiveExtensions) +
            ', ' + this.listToString('outArchiveFilePatterns', this.outArchiveFilePatterns) +
            ', ' + this.listToString('outDirPatterns', this.outDirPatterns) +
            ', ' + this.listToString('outExtensions', this.outExtensions) +
            ', ' + this.listToString('outFilePatterns', this.outFilePatterns) +
            ', ' + this.fileTypesToString('outFileTypes', this.outFileTypes) +
            ', ' + this.listToString('outLinesAfterPatterns', this.outLinesAfterPatterns) +
            ', ' + this.listToString('outLinesBeforePatterns', this.outLinesBeforePatterns) +
            ', ' + this.listToString('paths', this.paths) +
            ', printResults=' + this.printResults +
            ', printVersion=' + this.printVersion +
            ', recursive=' + this.recursive +
            ', searchArchives=' + this.searchArchives +
            ', ' + this.listToString('searchPatterns', this.searchPatterns) +
            ', textFileEncoding="' + this.textFileEncoding + '"' +
            ', uniqueLines=' + this.uniqueLines +
            ', verbose=' + this.verbose +
            ')';
    }
}

exports.SearchSettings = SearchSettings;
