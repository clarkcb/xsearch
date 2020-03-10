/*
 * searcher.js
 *
 * performs the searching based on the given SearchSettings instance
 */

const assert = require('assert');
const fs = require('fs');
const path = require('path');

const common = require('./common');
const FileType = require('./filetype').FileType;
const FileTypes = require('./filetypes').FileTypes;
const FileUtil = require('./fileutil').FileUtil;
const SearchFile = require('./searchfile').SearchFile;
const SearchResult = require('./searchresult').SearchResult;

class Searcher {
    "use strict";

    constructor(settings) {
        this.settings = settings;
        this.binaryEncoding = 'latin1';
        // from https://github.com/nodejs/node/blob/master/lib/buffer.js
        this.supportedEncodings = ['utf-8', 'utf8', 'latin1', 'ascii', 'ucs2',  'ucs-2', 'utf16le',
            'binary', 'base64', 'hex'];
        this.filetypes = new FileTypes();
        this.results = [];

        this.validateSettings();
    }

    validateSettings() {
        assert.ok(this.settings.startPath, 'Startpath not defined');
        assert.ok(fs.existsSync(this.settings.startPath), 'Startpath not found');
        try {
            fs.accessSync(this.settings.startPath, fs.constants.R_OK);
        } catch (accessErr) {
            if (accessErr.code === 'EACCES') {
                assert.ok(false, 'Startpath not readable');
            }
            throw accessErr;
        }
        let stat = fs.lstatSync(this.settings.startPath);
        if (stat.isDirectory()) {
            assert.ok(this.isSearchDir(this.settings.startPath),
                'Startpath does not match search settings');
        } else if (stat.isFile()) {
            assert.ok(this.filterFile(this.settings.startPath),
                'Startpath does not match search settings');
        } else {
            assert.ok(false, 'Startpath not readable file type');
        }
        assert.ok(this.settings.searchPatterns.length, 'No search patterns defined');
        assert.ok(this.supportedEncodings.indexOf(this.settings.textFileEncoding) > -1,
            'Invalid encoding');
        assert.ok(this.settings.linesBefore > -1, 'Invalid linesbefore');
        assert.ok(this.settings.linesAfter > -1, 'Invalid linesafter');
        assert.ok(this.settings.maxLineLength > -1, 'Invalid maxlinelength');
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
        let filetype = this.filetypes.getFileType(file);
        return !((this.settings.inFileTypes.length &&
            !this.matchesAnyElement(filetype, this.settings.inFileTypes))
            || (this.settings.outFileTypes.length &&
                this.matchesAnyElement(filetype, this.settings.outFileTypes)));
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

    getSearchFiles(startPath) {
        let searchFiles = [];
        let stats = fs.statSync(startPath);
        if (stats.isDirectory()) {
            if (this.isSearchDir(startPath)) {
                searchFiles = searchFiles.concat(this.recGetSearchFiles(startPath));
            } else {
                common.log("Warning: startPath does not match search criteria");
            }
        } else if (stats.isFile()) {
            const dirname = path.dirname(startPath) || '.';
            if (this.isSearchDir(dirname) && this.filterFile(startPath)) {
                const filename = path.basename(startPath);
                const filetype = this.filetypes.getFileType(filename);
                const sf = new SearchFile(dirname, filename, filetype);
                searchFiles.push(sf);
            } else {
                common.log("Warning: startPath does not match search criteria");
            }
        }
        return searchFiles;
    }

    recGetSearchFiles(currentDir) {
        let searchDirs = [];
        let searchFiles = [];
        fs.readdirSync(currentDir).map(f => {
            return path.join(currentDir, f);
        }).forEach(f => {
            let stats = fs.statSync(f);
            if (stats.isDirectory() && this.settings.recursive && this.isSearchDir(f)) {
                searchDirs.push(f);
            } else if (stats.isFile() && this.filterFile(f)) {
                const dirname = path.dirname(f) || '.';
                const filename = path.basename(f);
                const filetype = this.filetypes.getFileType(filename);
                const sf = new SearchFile(dirname, filename, filetype);
                searchFiles.push(sf);
            }
        });
        searchDirs.forEach(d => {
            [].push.apply(searchFiles, this.recGetSearchFiles(d));
        });
        return searchFiles;
    }

    filterFile(f) {
        if (this.filetypes.isArchiveFile(f)) {
            return (this.settings.searchArchives && this.isArchiveSearchFile(f));
        }
        return (!this.settings.archivesOnly && this.isSearchFile(f));
    }

    search() {
        // get the search files
        let searchfiles = this.getSearchFiles(this.settings.startPath);
        if (this.settings.verbose) {
            let dirs = searchfiles.map(sf => sf.pathname);
            dirs = common.setFromArray(dirs);
            dirs.sort();
            common.log("\nDirectories to be searched " + `(${dirs.length}):`);
            dirs.forEach(d => common.log(d));

            common.log("\nFiles to be searched " + `(${searchfiles.length}):`);
            searchfiles.forEach(sf => common.log(sf.relativePath()));
            common.log("");
        }

        // search the files
        searchfiles.forEach(f => this.searchFile(f));

        if (this.settings.verbose) {
            common.log("Search complete.");
        }
    }

    searchFile(searchfile) {
        switch (searchfile.filetype) {
            case FileType.CODE:
            case FileType.TEXT:
            case FileType.XML:
                this.searchTextFile(searchfile);
                break;
            case FileType.BINARY:
                this.searchBinaryFile(searchfile);
                break;
            default:
                // TODO: add message about unsupported filetype
                break;
        }
    }

    searchBinaryFile(searchfile) {
        if (this.settings.verbose) {
            common.log(`Searching binary file: "${searchfile}"`);
        }
        let contents = FileUtil.getFileContents(searchfile.relativePath(), this.binaryEncoding);
        let pattern = '';
        let patternResults = {};
        this.settings.searchPatterns.forEach(p => {
            pattern = new RegExp(p.source, "g");
            if (this.settings.firstMatch && (pattern.source in patternResults)) {
                return;
            }
            let match = pattern.exec(contents);
            while (match) {
                this.addSearchResult(new SearchResult(
                    pattern,
                    searchfile,
                    0,
                    match.index+1,
                    pattern.lastIndex+1,
                    null,
                    [],
                    []));
                if (this.settings.firstMatch) {
                    patternResults[pattern.source] = 1;
                    break;
                }
                match = pattern.exec(contents);
            }
        });
    }

    searchTextFile(searchfile) {
        if (this.settings.verbose) {
            common.log(`Searching text file ${searchfile}`);
        }
        if (this.settings.multilineSearch) {
            this.searchTextFileContents(searchfile);
        } else {
            this.searchTextFileLines(searchfile);
        }
    }

    searchTextFileContents(searchfile) {
        let contents = FileUtil.getFileContents(searchfile.relativePath(), this.settings.textFileEncoding);
        let results = this.searchMultiLineString(contents);
        results.forEach(r => {
            let resultWithFilepath =
                new SearchResult(r.pattern, searchfile, r.linenum,
                    r.matchStartIndex, r.matchEndIndex, r.line,
                    r.linesBefore, r.linesAfter);
            this.addSearchResult(resultWithFilepath);
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
        return (i) => { return i <= matchVal; };
    }

    getGreaterThan(matchVal) {
        return (i) => { return i > matchVal; };
    }

    plusOne(i) {
        return i + 1;
    }

    searchMultiLineString(s) {
        let patternResults = {};
        let linesBefore = [];
        let linesAfter = [];
        let results = [];
        let newLineIndices = this.getNewLineIndices(s);
        let startLineIndices = [0].concat(newLineIndices.map(this.plusOne));
        let endLineIndices = newLineIndices.concat([s.length - 1]);
        this.settings.searchPatterns.forEach(p => {
            let pattern = new RegExp(p.source, "g");
            let match = pattern.exec(s);
            let stop = false;
            while (match && !stop) {
                if (this.settings.firstMatch && pattern.source in patternResults) {
                    stop = true;
                    continue;
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
                    let searchResult = new SearchResult(
                        pattern,
                        '',
                        beforeLineCount+1,
                        matchStartIndex,
                        matchEndIndex,
                        line,
                        [].concat(linesBefore),
                        [].concat(linesAfter));
                    results.push(searchResult);
                    if (!(pattern.source in patternResults)) {
                        patternResults[pattern.source] = 1;
                    }
                }
                match = pattern.exec(s);
            }
        });
        return results;
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

    searchTextFileLines(searchfile) {
        let lines = FileUtil.getFileLines(searchfile.relativePath(), this.settings.textFileEncoding);
        let results = this.searchLines(lines);
        results.forEach(r => {
            let resultWithFilepath =
                new SearchResult(r.pattern, searchfile, r.linenum,
                    r.matchStartIndex, r.matchEndIndex, r.line,
                    r.linesBefore, r.linesAfter);
            this.addSearchResult(resultWithFilepath);
        });
    }

    // return results so that filepath can be added to them
    searchLines(lines) {
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
                    if ((this.settings.linesBefore === 0 || linesBeforeMatch(linesBefore)) &&
                        (this.settings.linesAfter === 0 || linesAfterMatch(linesAfter))) {
                        results.push(new SearchResult(
                            pattern,
                            '',
                            linenum,
                            match.index+1,
                            pattern.lastIndex+1,
                            line,
                            [].concat(linesBefore),
                            [].concat(linesAfter)));
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

    addSearchResult(result) {
        this.results.push(result);
    }

    cmpSearchResults(r1, r2) {
        const pathCmp = r1.file.pathname.localeCompare(r2.file.pathname);
        if (pathCmp === 0) {
            const fileCmp = path.basename(r1.file.filename).localeCompare(path.basename(r2.file.filename));
            if (fileCmp === 0) {
                if (r1.linenum === r2.linenum) {
                    return r1.matchStartIndex - r2.matchStartIndex;
                }
                return r1.linenum - r2.linenum;
            }
            return fileCmp;
        }
        return pathCmp;
    }

    printSearchResults() {
        // first sort the results
        this.results.sort(this.cmpSearchResults);
        common.log("\nSearch results " + `(${this.results.length}):`);
        this.results.forEach(r => common.log(r.toString()));
    }

    getMatchingDirs() {
        const dirs = this.results.map(r => path.dirname(r.filename));
        return common.setFromArray(dirs);
    }

    printMatchingDirs() {
        const dirs = this.getMatchingDirs();
        common.log("\nDirectories with matches " + `(${dirs.length}):`);
        dirs.forEach(d => common.log(d));
    }

    getMatchingFiles() {
        const files = this.results.map(r => r.filename);
        return common.setFromArray(files);
    }

    printMatchingFiles() {
        const files = this.getMatchingFiles();
        common.log("\nFiles with matches " + `(${files.length}):`);
        files.forEach(f => common.log(f));
    }

    getMatchingLines() {
        let lines = this.results.filter(r => r.linenum > 0).map(r => r.line.trim());
        if (this.settings.uniqueLines) {
            lines = common.setFromArray(lines);
        }
        lines.sort((a, b) => {
            if (a.toUpperCase() === b.toUpperCase())
                return 0;
            return a.toUpperCase() < b.toUpperCase() ? -1 : 1;
        });
        return lines;
    }

    printMatchingLines() {
        const lines = this.getMatchingLines();
        let hdrText;
        if (this.settings.uniqueLines)
            hdrText = "\nUnique lines with matches " + `(${lines.length}):`;
        else
            hdrText = "\nLines with matches " + `(${lines.length}):`;
        common.log(hdrText);
        lines.forEach(l => common.log(l));
    }
}

exports.Searcher = Searcher;
