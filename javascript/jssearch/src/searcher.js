/*
 * searcher.js
 *
 * performs the searching based on the given SearchSettings instance
 */

const assert = require('assert');
const {common, FileType, FileUtil, Finder} = require('jsfind');
const {SearchError} = require('./searcherror');
const {SearchResult} = require('./searchresult');
const path = require('path')

class Searcher {
    'use strict'

    constructor(settings) {
        this.settings = settings;
        this.binaryEncoding = 'latin1';
        // from https://github.com/nodejs/node/blob/master/lib/buffer.js
        this.supportedEncodings = ['utf-8', 'utf8', 'latin1', 'ascii', 'ucs2',  'ucs-2', 'utf16le',
            'binary', 'base64', 'hex'];
        this.finder = new Finder(settings.findSettings);
        this.validateSettings();
    }

    validateSettings() {
        try {
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

    async search() {
        try {
            // get the search files
            let fileResults = await this.finder.find();
    
            if (this.settings.verbose) {
                let dirs = fileResults.map(sf => sf.path);
                dirs = common.setFromArray(dirs);
                dirs.sort();
                common.log("\nDirectories to be searched " + `(${dirs.length}):`);
                dirs.forEach(d => common.log(d));

                common.log("\nFiles to be searched " + `(${fileResults.length}):`);
                fileResults.forEach(sf => common.log(sf.relativePath()));
                common.log("");
            }

            // search the files
            let results = [];
            const searchResultsArrays = await Promise.all(fileResults.map(fr => this.searchFile(fr)));
            searchResultsArrays.forEach(searchResults => {
                results = results.concat(searchResults);
            });

            if (this.settings.verbose) {
                common.log('Search complete.');
            }

            return results;

        } catch (err) {
            throw err;
        }
    }

    async searchFile(fileResult) {
        let results = [];
        switch (fileResult.fileType) {
            case FileType.CODE:
            case FileType.TEXT:
            case FileType.XML:
                results = await this.searchTextFile(fileResult);
                break;
            case FileType.BINARY:
                results = await this.searchBinaryFile(fileResult);
                break;
            default:
                // TODO: add message about unsupported fileType
                break;
        }
        return results;
    }

    async searchBinaryFile(fileResult) {
        if (this.settings.verbose) {
            common.log(`Searching binary file: "${fileResult}"`);
        }

        const contents = FileUtil.getFileContentsSync(fileResult.relativePath(), this.binaryEncoding);
        let results = [];

        const searchPattern = p => {
            let pattern = new RegExp(p.source, 'g');
            let patternResults = [];
            let match = pattern.exec(contents);
            while (match) {
                patternResults.push(new SearchResult(
                    pattern,
                    fileResult,
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

    async searchTextFile(fileResult) {
        if (this.settings.verbose) {
            common.log(`Searching text file ${fileResult}`);
        }
        let results;
        if (this.settings.multilineSearch) {
            results = await this.searchTextFileContents(fileResult);
        } else {
            results = await this.searchTextFileLines(fileResult);
        }
        return results;
    }

    async searchTextFileContents(fileResult) {
        const contents = FileUtil.getFileContentsSync(fileResult.relativePath(), this.settings.textFileEncoding);
        let stringResults = await this.searchMultiLineString(contents);
        return stringResults.map(r => {
            return new SearchResult(r.pattern, fileResult, r.lineNum, r.matchStartIndex, r.matchEndIndex, r.line,
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
                            null,
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

    async searchTextFileLines(fileResult) {
        let lines = FileUtil.getFileLinesSync(fileResult.relativePath(), this.settings.textFileEncoding);
        let linesResults = await this.searchLines(lines);
        return linesResults.map(r => {
            return new SearchResult(r.pattern, fileResult, r.lineNum, r.matchStartIndex, r.matchEndIndex, r.line,
                r.linesBefore, r.linesAfter, this.settings.maxLineLength, this.settings.colorize);
        });
    }

    // return results so that filepath can be added to them
    async searchLines(lines) {
        let lineNum = 0;
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
            lineNum += 1;
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
                            null,
                            lineNum,
                            match.index+1,
                            pattern.lastIndex+1,
                            line,
                            [...linesBefore],
                            [...linesAfter],
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

    cmpSearchResults(r1, r2) {
        const pathCmp = r1.file.path.localeCompare(r2.file.path);
        if (pathCmp === 0) {
            const fileCmp = path.basename(r1.file.fileName).localeCompare(path.basename(r2.file.fileName));
            if (fileCmp === 0) {
                if (r1.lineNum === r2.lineNum) {
                    return r1.matchStartIndex - r2.matchStartIndex;
                }
                return r1.lineNum - r2.lineNum;
            }
            return fileCmp;
        }
        return pathCmp;
    }

    printSearchResults(results, formatter) {
        // first sort the results
        // TODO: ensure sorting gets help from jsfind
        results.sort(this.cmpSearchResults);
        common.log(`\nSearch results (${results.length}):`);
        results.forEach(r => common.log(formatter.format(r)));
    }

    getFileResults(results) {
        let fileMap = {};
        let fileResults = [];
        for (let r of results) {
            if (!fileMap[r.file.relativePath()]) {
                fileMap[r.file.relativePath()] = r.file;
                fileResults.push(r.file);
            }
        }
        return fileResults;
    }

    printMatchingDirs(results, formatter) {
        const fileResults = this.getFileResults(results);
        this.finder.printMatchingDirs(fileResults, formatter.fileFormatter);
    }

    printMatchingFiles(results, formatter) {
        const fileResults = this.getFileResults(results);
        this.finder.printMatchingFiles(fileResults, formatter.fileFormatter);
    }

    getMatchingLines(results) {
        let lines = results.filter(r => r.lineNum > 0).map(r => r.line.trim());
        if (this.settings.uniqueLines) {
            lines = common.setFromArray(lines);
        }
        lines.sort((a, b) => {
            let aUpper = a.toUpperCase();
            let bUpper = b.toUpperCase();
            if (aUpper === bUpper)
                return 0;
            return aUpper < bUpper ? -1 : 1;
        });
        return lines;
    }

    printMatchingLines(results, formatter) {
        const lines = this.getMatchingLines(results);
        let hdrText;
        if (this.settings.uniqueLines)
            hdrText = `\nUnique lines with matches`;
        else
            hdrText = `\nLines with matches`;
        if (lines.length > 0) {
            hdrText = `${hdrText} (${lines.length}):`;
            common.log(hdrText);
            lines.forEach(l => common.log(formatter.formatLine(l)));
        } else {
            hdrText = `${hdrText}: 0`;
            common.log(hdrText);
        }
    }
}

exports.Searcher = Searcher;
