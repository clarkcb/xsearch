/*
 * searcher.js
 *
 * performs the searching based on the given SearchSettings instance
 */

const assert = require('assert');
const fs = require('fs');
const fsp = fs.promises;
const path = require('path');
const { promisify } = require('util');
const fsStatAsync = promisify(fs.stat);
const fsReaddirAsync = promisify(fs.readdir);

const common = require('./common');
const {FileType} = require('./filetype');
const {FileTypes} = require('./filetypes');
const FileUtil = require('./fileutil');
const {SearchError} = require('./searcherror');
const {SearchFile} = require('./searchfile');
const {SearchResult} = require('./searchresult');

class Searcher {
    'use strict'

    constructor(settings) {
        this.settings = settings;
        this.binaryEncoding = 'latin1';
        // from https://github.com/nodejs/node/blob/master/lib/buffer.js
        this.supportedEncodings = ['utf-8', 'utf8', 'latin1', 'ascii', 'ucs2',  'ucs-2', 'utf16le',
            'binary', 'base64', 'hex'];
        this.fileTypes = new FileTypes();
        this.results = [];
        this.validateSettings();
    }

    validateSettings() {
        try {
            assert.ok(this.settings.paths.length > 0, 'Startpath not defined');
            this.settings.paths.forEach(p => {
                fs.accessSync(p, fs.constants.F_OK | fs.constants.R_OK);

                const stat = fs.lstatSync(p);
                if (stat.isDirectory()) {
                    assert.ok(this.isSearchDir(p),
                        'Startpath does not match search settings');
                } else if (stat.isFile()) {
                    assert.ok(this.filterFile(p),
                        'Startpath does not match search settings');
                } else {
                    assert.ok(false, 'Startpath not searchable file type');
                }
            });
            assert.ok(this.settings.searchPatterns.length, 'No search patterns defined');
            assert.ok(this.supportedEncodings.indexOf(this.settings.textFileEncoding) > -1,
                'Invalid encoding');
            assert.ok(this.settings.linesBefore > -1, 'Invalid linesbefore');
            assert.ok(this.settings.linesAfter > -1, 'Invalid linesafter');
            assert.ok(this.settings.maxLineLength > -1, 'Invalid maxlinelength');

        } catch (err) {
            let msg = err.message;
            if (err.code === 'ENOENT') {
                msg = 'Startpath not found';
            } else if (err.code === 'EACCES') {
                msg = 'Startpath not readable';
            }
            throw new SearchError(msg);
        }
    }

    matchesAnyElement(s, elements) {
        return elements.indexOf(s) > -1;
    }

    matchesAnyPattern(s, patterns) {
        return patterns.some((p, i, arr) => s.search(p) > -1);
    }

    anyMatchesAnyPattern(ss, patterns) {
        return ss.some((s, i, arr) => this.matchesAnyPattern(s, patterns));
    }

    isSearchDir(dir) {
        if (FileUtil.isDotDir(dir)) {
            return true;
        }
        if (this.settings.excludeHidden) {
            let nonDotElems = dir.split(path.sep).filter(p => !this.matchesAnyElement(p, ['.','..']));
            if (nonDotElems.length === 0) {
                return true;
            }
            if (nonDotElems.some((p, i, arr) => FileUtil.isHidden(p))) {
                return false;
            }
        }
        if (this.settings.inDirPatterns.length && !this.matchesAnyPattern(dir,
            this.settings.inDirPatterns)) {
            return false;
        }
        return !(this.settings.outDirPatterns.length && this.matchesAnyPattern(dir,
            this.settings.outDirPatterns));
    }

    isSearchFile(file) {
        if (FileUtil.isHidden(file) && this.settings.excludeHidden) {
            return false;
        }
        let ext = FileUtil.getExtension(file);
        if ((this.settings.inExtensions.length &&
            !this.matchesAnyElement(ext, this.settings.inExtensions))
            || (this.settings.outExtensions.length &&
                this.matchesAnyElement(ext, this.settings.outExtensions))
            || (this.settings.inFilePatterns.length &&
                !this.matchesAnyPattern(file, this.settings.inFilePatterns))
            || (this.settings.outFilePatterns.length &&
                this.matchesAnyPattern(file, this.settings.outFilePatterns))) {
            return false;
        }
        let fileType = this.fileTypes.getFileType(file);
        return !((this.settings.inFileTypes.length &&
            !this.matchesAnyElement(fileType, this.settings.inFileTypes))
            || (this.settings.outFileTypes.length &&
                this.matchesAnyElement(fileType, this.settings.outFileTypes)));
    }

    isArchiveSearchFile(file) {
        if (FileUtil.isHidden(file) && this.settings.excludeHidden) {
            return false;
        }
        let ext = FileUtil.getExtension(file);
        if (this.settings.inArchiveExtensions.length &&
            !this.matchesAnyElement(ext, this.settings.inArchiveExtensions)) {
            return false;
        }
        if (this.settings.outArchiveExtensions.length &&
            this.matchesAnyElement(ext, this.settings.outArchiveExtensions)) {
            return false;
        }
        if (this.settings.inArchiveFilePatterns.length &&
            !this.matchesAnyPattern(file, this.settings.inArchiveFilePatterns)) {
            return false;
        }
        return !(this.settings.outArchiveFilePatterns.length &&
            this.matchesAnyPattern(file, this.settings.outArchiveFilePatterns));
    }

    async getSearchFiles(startPath) {
        let searchFiles = [];
        try {
            let stats = await fsStatAsync(startPath);
            if (stats.isDirectory()) {
                if (this.isSearchDir(startPath)) {
                    searchFiles = await this.recGetSearchFiles(startPath);
                } else {
                    throw new SearchError("startPath does not match search criteria");
                }
            } else if (stats.isFile()) {
                const dirName = path.dirname(startPath) || '.';
                if (this.isSearchDir(dirName) && this.filterFile(startPath)) {
                    const fileName = path.basename(startPath);
                    const fileType = this.fileTypes.getFileType(fileName);
                    const sf = new SearchFile(dirName, fileName, fileType);
                    searchFiles.push(sf);
                } else {
                    throw new SearchError("startPath does not match search criteria");
                }
            }
            return searchFiles;

        } catch (err) {
            // common.log(err);
            throw err;
        }
    }

    async recGetSearchFiles(currentDir) {
        let searchDirs = [];
        let searchFiles = [];
        let files = await fsReaddirAsync(currentDir);
        files.map(f => {
            return path.join(currentDir, f);
        }).forEach(f => {
            let stats = fs.statSync(f);
            if (stats.isDirectory() && this.settings.recursive && this.isSearchDir(f)) {
                searchDirs.push(f);
            } else if (stats.isFile() && this.filterFile(f)) {
                const dirName = path.dirname(f) || '.';
                const fileName = path.basename(f);
                const fileType = this.fileTypes.getFileType(fileName);
                const sf = new SearchFile(dirName, fileName, fileType);
                searchFiles.push(sf);
            }
        });

        const subDirSearchFileArrays = await Promise.all(searchDirs.map(d => this.recGetSearchFiles(d)));
        subDirSearchFileArrays.forEach(subDirSearchFiles => {
            searchFiles = searchFiles.concat(subDirSearchFiles);
        });
        return searchFiles;
    }

    filterFile(f) {
        if (this.fileTypes.isArchiveFile(f)) {
            return (this.settings.searchArchives && this.isArchiveSearchFile(f));
        }
        return (!this.settings.archivesOnly && this.isSearchFile(f));
    }

    async search() {
        try {
            // get the search files
            const pathSearchFilesArrays = await Promise.all(this.settings.paths.map(d => this.getSearchFiles(d)));
            // let searchfiles = await this.getSearchFiles(this.settings.startPath);
            let searchFiles = [];
            pathSearchFilesArrays.forEach(pathSearchFiles => {
                searchFiles = searchFiles.concat(pathSearchFiles);
            });
    
            if (this.settings.verbose) {
                let dirs = searchFiles.map(sf => sf.pathname);
                dirs = common.setFromArray(dirs);
                dirs.sort();
                common.log("\nDirectories to be searched " + `(${dirs.length}):`);
                dirs.forEach(d => common.log(d));

                common.log("\nFiles to be searched " + `(${searchFiles.length}):`);
                searchFiles.forEach(sf => common.log(sf.relativePath()));
                common.log("");
            }

            // search the files
            let results = [];
            const searchFileResultsArrays = await Promise.all(searchFiles.map(sf => this.searchFile(sf)));
            searchFileResultsArrays.forEach(searchFileResults => {
                results = results.concat(searchFileResults);
            });

            if (this.settings.verbose) {
                common.log('Search complete.');
            }

            return results;

        } catch (err) {
            throw err;
        }
    }

    async searchFile(searchfile) {
        let results = [];
        switch (searchfile.fileType) {
            case FileType.CODE:
            case FileType.TEXT:
            case FileType.XML:
                results = await this.searchTextFile(searchfile);
                break;
            case FileType.BINARY:
                results = await this.searchBinaryFile(searchfile);
                break;
            default:
                // TODO: add message about unsupported fileType
                break;
        }
        return results;
    }

    async searchBinaryFile(searchfile) {
        if (this.settings.verbose) {
            common.log(`Searching binary file: "${searchfile}"`);
        }

        const contents = await FileUtil.getFileContentsAsync(searchfile.relativePath(), this.binaryEncoding);
        let results = [];

        const searchPattern = pattern => {
            pattern = new RegExp(pattern.source, 'g');
            let patternResults = [];
            let match = pattern.exec(contents);
            while (match) {
                patternResults.push(new SearchResult(
                    pattern,
                    searchfile,
                    0,
                    match.index+1,
                    pattern.lastIndex+1,
                    null,
                    [],
                    []));
                if (this.settings.firstMatch) {
                    return patternResults;
                }
                match = pattern.exec(contents);
            }
            return patternResults;
        }

        const patternResultArrays = await Promise.all(this.settings.searchPatterns.map(p => searchPattern(p)));
        patternResultArrays.forEach(patternResults => {
            results = results.concat(patternResults);
        });
        return results;
    }

    async searchTextFile(searchfile) {
        if (this.settings.verbose) {
            common.log(`Searching text file ${searchfile}`);
        }
        let results;
        if (this.settings.multilineSearch) {
            results = await this.searchTextFileContents(searchfile);
        } else {
            results = await this.searchTextFileLines(searchfile);
        }
        return results;
    }

    async searchTextFileContents(searchfile) {
        const contents = await FileUtil.getFileContentsAsync(searchfile.relativePath(), this.settings.textFileEncoding);
        let stringResults = await this.searchMultiLineString(contents);
        return stringResults.map(r => {
            return new SearchResult(r.pattern, searchfile, r.linenum, r.matchStartIndex, r.matchEndIndex, r.line,
                r.linesBefore, r.linesAfter, this.settings.maxLineLength, this.settings.colorize);
        });
    }

    getNewLineIndices(s) {
        let indices = [];
        for (let i = 0; i < s.length; i++) {
            if (s.charAt(i) === "\n") {
                indices.push(i);
            }
        }
        return indices;
    }

    getLinesAtIndices(s, atIndices, startLineIndices, endLineIndices) {
        if (atIndices.length === 0)
            return [];
        let lines = [];
        atIndices.forEach(i => {
            let line = s.substring(i, endLineIndices[startLineIndices.indexOf(i)]);
            lines.push(line);
        });
        return lines;
    }

    getLinesBefore(s, beforeStartIndices, startLineIndices, endLineIndices) {
        return this.getLinesAtIndices(s, beforeStartIndices, startLineIndices, endLineIndices);
    }

    getLinesAfter(s, afterStartIndices, startLineIndices, endLineIndices) {
        return this.getLinesAtIndices(s, afterStartIndices, startLineIndices, endLineIndices);
    }

    getLessThanOrEqual(matchVal) {
        return i => { return i <= matchVal; };
    }

    getGreaterThan(matchVal) {
        return i => { return i > matchVal; };
    }

    plusOne(i) {
        return i + 1;
    }

    async searchMultiLineString(s) {
        let linesBefore = [];
        let linesAfter = [];
        let results = [];
        try {
            let newLineIndices = this.getNewLineIndices(s);
            let startLineIndices = [0].concat(newLineIndices.map(this.plusOne));
            let endLineIndices = newLineIndices.concat([s.length - 1]);

            const searchPattern = pattern => {
                pattern = new RegExp(pattern.source, 'g');
                let patternResults = [];
                let match = pattern.exec(s);
                while (match) {
                    if (this.settings.firstMatch && patternResults.length > 0) {
                        return patternResults;
                    }
                    let lessOrEqual = this.getLessThanOrEqual(match.index);
                    let greaterThan = this.getGreaterThan(match.index);
                    let lineStartIndex = 0;
                    let lineEndIndex = s.length - 1;
                    let beforeLineCount = 0;
                    let beforeStartIndices = startLineIndices.filter(lessOrEqual);
                    if (beforeStartIndices.length > 0) {
                        lineStartIndex = beforeStartIndices.pop();
                        beforeLineCount = beforeStartIndices.length;
                        if (beforeStartIndices.length > this.settings.linesBefore) {
                            beforeStartIndices = beforeStartIndices.slice(
                                beforeStartIndices.length - this.settings.linesBefore);
                        }
                    }
                    lineEndIndex = endLineIndices[startLineIndices.indexOf(lineStartIndex)];
                    let line = s.substring(lineStartIndex, lineEndIndex);
                    if (this.settings.linesBefore && beforeLineCount) {
                        linesBefore = this.getLinesBefore(s, beforeStartIndices,
                            startLineIndices, endLineIndices);
                    }
                    if (this.settings.linesAfter) {
                        let afterStartIndices = startLineIndices.filter(greaterThan);
                        if (afterStartIndices.length > this.settings.linesAfter) {
                            afterStartIndices = afterStartIndices.slice(0,
                                this.settings.linesAfter);
                        }
                        linesAfter = this.getLinesAfter(s, afterStartIndices,
                            startLineIndices, endLineIndices);
                    }
                    let matchStartIndex = match.index - lineStartIndex + 1;
                    let matchEndIndex = pattern.lastIndex - lineStartIndex + 1;
                    if ((this.settings.linesBefore === 0 || this.linesBeforeMatch(linesBefore)) &&
                        (this.settings.linesAfter === 0 || this.linesAfterMatch(linesAfter))) {
                        patternResults.push(new SearchResult(
                            pattern,
                            '',
                            beforeLineCount + 1,
                            matchStartIndex,
                            matchEndIndex,
                            line,
                            [].concat(linesBefore),
                            [].concat(linesAfter),
                            this.settings.maxLineLength,
                            this.settings.colorize));
                        if (!(pattern.source in patternResults)) {
                            patternResults[pattern.source] = 1;
                        }
                    }
                    match = pattern.exec(s);
                }
                return patternResults;
            }

            const patternResultArrays = await Promise.all(this.settings.searchPatterns.map(p => searchPattern(p)));
            patternResultArrays.forEach(patternResults => {
                results = results.concat(patternResults);
            });
            return results;

        } catch (err) {
            throw err;
        }
    }

    linesMatch(lines, inPatterns, outPatterns) {
        return ((inPatterns.length === 0 || this.anyMatchesAnyPattern(lines, inPatterns)) &&
            (outPatterns.length === 0 || ! this.anyMatchesAnyPattern(lines, outPatterns)));
    }

    linesBeforeMatch(linesBefore) {
        return this.linesMatch(linesBefore, this.settings.inLinesBeforePatterns,
            this.settings.outLinesBeforePatterns);
    }

    linesAfterMatch(linesAfter) {
        return this.linesMatch(linesAfter, this.settings.inLinesAfterPatterns,
            this.settings.outLinesAfterPatterns);
    }

    async searchTextFileLines(searchfile) {
        let lines = FileUtil.getFileLines(searchfile.relativePath(), this.settings.textFileEncoding);
        let linesResults = await this.searchLines(lines);
        return linesResults.map(r => {
            return new SearchResult(r.pattern, searchfile, r.linenum, r.matchStartIndex, r.matchEndIndex, r.line,
                r.linesBefore, r.linesAfter, this.settings.maxLineLength, this.settings.colorize);
        });
    }

    // return results so that filepath can be added to them
    async searchLines(lines) {
        let linenum = 0;
        let pattern;
        let linesBefore = [];
        let linesAfter = [];
        let results = [];
        let patternResults = {};
        while (true) {
            if (Object.keys(patternResults).length === this.settings.searchPatterns.length) {
                break;
            }
            let line = "";
            if (linesAfter.length > 0) {
                line = linesAfter.shift();
            } else if (lines.length > 0) {
                line = lines.shift();
            } else {
                break;
            }
            linenum += 1;
            if (this.settings.linesAfter > 0) {
                while (linesAfter.length < this.settings.linesAfter && lines.length > 0) {
                    linesAfter.push(lines.shift());
                }
            }
            this.settings.searchPatterns.forEach(p => {
                pattern = new RegExp(p.source, "g");
                let match = pattern.exec(line);
                while (match) {
                    if ((this.settings.linesBefore === 0 || this.linesBeforeMatch(linesBefore)) &&
                        (this.settings.linesAfter === 0 || this.linesAfterMatch(linesAfter))) {
                        results.push(new SearchResult(
                            pattern,
                            '',
                            linenum,
                            match.index+1,
                            pattern.lastIndex+1,
                            line,
                            [].concat(linesBefore),
                            [].concat(linesAfter),
                            this.settings.maxLineLength,
                            this.settings.colorize));
                        if (this.settings.firstMatch) {
                            patternResults[pattern.source] = 1;
                            break;
                        }
                    }
                    match = pattern.exec(line);
                }
            });
            if (this.settings.linesBefore > 0) {
                if (linesBefore.length === this.settings.linesBefore)
                    linesBefore.shift();
                if (linesBefore.length < this.settings.linesBefore)
                    linesBefore.push(line);
            }
        }
        return results;
    }
}

exports.Searcher = Searcher;
