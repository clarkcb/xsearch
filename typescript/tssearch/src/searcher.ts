/*
 * searcher.ts
 *
 * performs the searching based on the given SearchSettings instance
 */

"use strict";

import * as assert from 'assert';
import * as common from './common';
import {FileResult, FileType, FileUtil, Finder} from 'tsfind';
import {SearchError} from './searcherror';
import {SearchResult} from './searchresult';
import {SearchSettings} from './searchsettings';
import {SearchResultFormatter} from "./searchresultformatter";

export class Searcher {
    _binaryEncoding: BufferEncoding = 'latin1';
    // from https://github.com/nodejs/node/blob/master/lib/buffer.js
    _supportedEncodings: string[] = ['utf-8', 'utf8', 'latin1', 'ascii', 'ucs2',  'ucs-2', 'utf16le',
        'binary', 'base64', 'hex'];

    _finder: Finder;
    _settings: SearchSettings;

    constructor(settings: SearchSettings) {
        this._finder = new Finder(settings.getFindSettings());
        this._settings = settings;
        this.validateSettings();
    }

    private validateSettings(): void {
        try {
            assert.ok(this._settings.searchPatterns.length, 'No search patterns defined');
            assert.ok(this._supportedEncodings.indexOf(this._settings.textFileEncoding) > -1,
                'Invalid encoding');
            assert.ok(this._settings.linesBefore > -1, 'Invalid linesbefore');
            assert.ok(this._settings.linesAfter > -1, 'Invalid linesafter');
            assert.ok(this._settings.maxLineLength > -1, 'Invalid maxlinelength');

        } catch (err: Error | any) {
            let msg = err.message;
            if (err.code === 'ENOENT') {
                msg = 'Startpath not found';
            } else if (err.code === 'EACCES') {
                msg = 'Startpath not readable';
            }
            throw new SearchError(msg);
        }
    }

    private static matchesAnyString(s: string, elements: string[]): boolean {
        return elements.indexOf(s) > -1;
    }

    private static matchesAnyPattern(s: string, patterns: RegExp[]): boolean {
        return patterns.some((p: RegExp) => s.search(p) > -1);
    }

    private static matchesAnyFileType(ft: FileType, fileTypes: FileType[]): boolean {
        return fileTypes.indexOf(ft) > -1;
    }

    private static anyMatchesAnyPattern(ss: string[], patterns: RegExp[]): boolean {
        return ss.some((s: string) => this.matchesAnyPattern(s, patterns));
    }

    public async search(): Promise<SearchResult[]> {
        // get the search files
        let fileResults: FileResult[] = await this._finder.find();

        if (this._settings.verbose) {
            let dirs = fileResults.map(sf => sf.path);
            dirs = common.setFromArray(dirs);
            dirs.sort();
            common.log("\nDirectories to be searched " + `(${dirs.length}):`);
            dirs.forEach(d => common.log(d));

            common.log("\nFiles to be searched " + `(${fileResults.length}):`);
            fileResults.forEach(sf => common.log(sf.toString()));
            common.log("");
        }

        // search the files
        let results: SearchResult[] = [];
        const searchResultsArrays: SearchResult[][] = await Promise.all(fileResults.map(fr => this.searchFile(fr)));
        searchResultsArrays.forEach(searchResults => {
            results = results.concat(searchResults);
        });

        if (this._settings.verbose) {
            common.log('Search complete.');
        }

        return results;
    }

    private async searchFile(fileResult: FileResult): Promise<SearchResult[]> {
        let results: SearchResult[] = [];
        switch (fileResult.fileType) {
            case FileType.Code:
            case FileType.Xml:
            case FileType.Text:
                results = await this.searchTextFile(fileResult);
                break;
            case FileType.Binary:
                results = await this.searchBinaryFile(fileResult);
                break;
            default:
                // TODO: add message about unsupported filetype
                break;
        }
        return results;
    }

    private async searchBinaryFile(fileResult: FileResult): Promise<SearchResult[]> {
        const self = this;
        if (this._settings.verbose) {
            common.log(`Searching binary file: "${fileResult}"`);
        }
        const contents: string = FileUtil.getFileContentsSync(fileResult.relativePath(), this._binaryEncoding);
        let results: SearchResult[] = [];

        const searchPattern = (p: RegExp): SearchResult[] => {
            let pattern = new RegExp(p.source, 'g');
            let patternResults: SearchResult[] = [];
            let match = pattern.exec(contents);
            while (match) {
                patternResults.push(new SearchResult(
                    pattern,
                    fileResult,
                    0,
                    match.index + 1,
                    pattern.lastIndex + 1,
                    '',
                    [],
                    []));
                if (self._settings.firstMatch) {
                    return patternResults;
                }
                match = pattern.exec(contents);
            }
            return patternResults;
        }

        const patternResultArrays: SearchResult[][] = this._settings.searchPatterns.map((p: RegExp) => searchPattern(p));
        patternResultArrays.forEach(patternResults => {
            results = results.concat(patternResults);
        });
        return results;
    }

    private async searchTextFile(fileResult: FileResult): Promise<SearchResult[]> {
        if (this._settings.verbose) {
            common.log(`Searching text file ${fileResult}`);
        }
        let results: SearchResult[];
        if (this._settings.multilineSearch) {
            results = await this.searchTextFileContents(fileResult);
        } else {
            results = await this.searchTextFileLines(fileResult);
        }
        return results
    }

    private async searchTextFileContents(fileResult: FileResult): Promise<SearchResult[]> {
        const contents: string = FileUtil.getFileContentsSync(fileResult.relativePath(), this._settings.textFileEncoding);
        const results: SearchResult[] = await this.searchMultiLineString(contents);
        return results.map((r: SearchResult) => {
            return new SearchResult(r.pattern, fileResult, r.lineNum, r.matchStartIndex, r.matchEndIndex, r.line,
                r.linesBefore, r.linesAfter);
        });
    }

    private static getNewLineIndices(s: string): number[] {
        const indices: number[] = [];
        for (let i = 0; i < s.length; i++) {
            if (s.charAt(i) == "\n") {
                indices.push(i);
            }
        }
        return indices;
    }

    private static getLinesAtIndices(s: string, atIndices: number[],
                                     startLineIndices: number[],
                                     endLineIndices: number[]): string[] {
        if (atIndices.length === 0)
            return [];
        const lines: string[] = [];
        atIndices.forEach(function(i: number): void {
            const line: string = s.substring(i, endLineIndices[startLineIndices.indexOf(i)]);
            lines.push(line);
        });
        return lines;
    }

    private static getLinesBefore(s: string, beforeStartIndices: number[],
                                  startLineIndices: number[],
                                  endLineIndices: number[]): string[] {
        return Searcher.getLinesAtIndices(s, beforeStartIndices,
            startLineIndices, endLineIndices);
    }

    private static getLinesAfter(s: string, afterStartIndices: number[],
                                 startLineIndices: number[],
                                 endLineIndices: number[]): string[] {
        return Searcher.getLinesAtIndices(s, afterStartIndices,
            startLineIndices, endLineIndices);
    }

    private getLessThanOrEqual(matchIdx: number) {
        return (i: number): boolean => i <= matchIdx;
    }

    private getGreaterThan(matchIdx: number) {
        return (i: number): boolean => i > matchIdx;
    }

    public async searchMultiLineString(s: string): Promise<SearchResult[]> {
        const self = this;
        const patternResults: {[index: string]:number} = {};
        let linesBefore: string[] = [];
        let linesAfter: string[] = [];
        const results: SearchResult[] = [];
        const newLineIndices: number[] = Searcher.getNewLineIndices(s);
        const plusOne = (i: number): number => i+1;
        const startLineIndices: number[] = [0].concat(newLineIndices.map(plusOne));
        const endLineIndices: number[] = newLineIndices.concat([s.length - 1]);

        this._settings.searchPatterns.forEach((p: RegExp) => {
            const pattern: RegExp = new RegExp(p.source, "g");
            let match = pattern.exec(s);
            let stop = false;
            while (match && !stop) {
                if (self._settings.firstMatch && pattern.source in patternResults) {
                    stop = true;
                    continue;
                }
                const lessOrEqual = self.getLessThanOrEqual(match.index);
                const greaterThan = self.getGreaterThan(match.index);
                let lineStartIndex = 0;
                let lineEndIndex = s.length - 1;
                let beforeLineCount = 0;
                let beforeStartIndices: number[] = startLineIndices.filter(lessOrEqual);
                if (beforeStartIndices.length > 0) {
                    lineStartIndex = beforeStartIndices.pop() || -1;
                    beforeLineCount = beforeStartIndices.length;
                    if (beforeStartIndices.length > self._settings.linesBefore) {
                        beforeStartIndices = beforeStartIndices.slice(
                            beforeStartIndices.length - self._settings.linesBefore);
                    }
                }
                lineEndIndex = endLineIndices[startLineIndices.indexOf(lineStartIndex)];
                const line: string = s.substring(lineStartIndex, lineEndIndex);
                if (self._settings.linesBefore && beforeLineCount) {
                    linesBefore = Searcher.getLinesBefore(s, beforeStartIndices,
                        startLineIndices, endLineIndices);
                }
                if (self._settings.linesAfter) {
                    let afterStartIndices: number[] = startLineIndices.filter(greaterThan);
                    if (afterStartIndices.length > self._settings.linesAfter) {
                        afterStartIndices = afterStartIndices.slice(0,
                            self._settings.linesAfter);
                    }
                    linesAfter = Searcher.getLinesAfter(s, afterStartIndices,
                        startLineIndices, endLineIndices);
                }
                const matchStartIndex: number = match.index - lineStartIndex + 1;
                const matchEndIndex: number = pattern.lastIndex - lineStartIndex + 1;
                if ((self._settings.linesBefore === 0 || self.linesBeforeMatch(linesBefore)) &&
                    (self._settings.linesAfter === 0 || self.linesAfterMatch(linesAfter))) {
                    const searchResult: SearchResult = new SearchResult(
                        pattern,
                        null,
                        beforeLineCount+1,
                        matchStartIndex,
                        matchEndIndex,
                        line,
                        linesBefore,
                        linesAfter);
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

    private static linesMatch(lines: string[], inPatterns: RegExp[], outPatterns: RegExp[]): boolean {
        return ((inPatterns.length === 0 || Searcher.anyMatchesAnyPattern(lines, inPatterns)) &&
               (outPatterns.length === 0 || ! Searcher.anyMatchesAnyPattern(lines, outPatterns)));
    }

    private linesBeforeMatch(linesBefore: string[]): boolean {
        return Searcher.linesMatch(linesBefore, this._settings.inLinesBeforePatterns,
            this._settings.outLinesBeforePatterns);
    }

    private linesAfterMatch(linesAfter: string[]): boolean {
        return Searcher.linesMatch(linesAfter, this._settings.inLinesAfterPatterns,
            this._settings.outLinesAfterPatterns);
    }

    private async searchTextFileLines(fileResult: FileResult): Promise<SearchResult[]> {
        const self = this;
        const lines: string[] = FileUtil.getFileLinesSync(fileResult.relativePath(), this._settings.textFileEncoding);
        const linesResults: SearchResult[] = await this.searchLines(lines);
        return linesResults.map((r: SearchResult) => {
            return new SearchResult(r.pattern, fileResult, r.lineNum, r.matchStartIndex, r.matchEndIndex, r.line,
                r.linesBefore, r.linesAfter);
        });
    }

    // return results so that filepath can be added to them
    public async searchLines(lines: string[]): Promise<SearchResult[]> {
        const self = this;
        let lineNum = 0;
        let pattern: RegExp;
        const linesBefore: string[] = [];
        const linesAfter: string[] = [];
        const results: SearchResult[] = [];
        const patternResults: {[index: string]:number} = {};
        while (true) {
            if (Object.keys(patternResults).length === this._settings.searchPatterns.length) {
                break;
            }
            let line = "";
            if (linesAfter.length > 0) {
                line = linesAfter.shift() || '';
            } else if (lines.length > 0) {
                line = lines.shift() || '';
            } else {
                break;
            }
            lineNum += 1;
            if (this._settings.linesAfter > 0) {
                while (linesAfter.length < this._settings.linesAfter && lines.length > 0) {
                    linesAfter.push(lines.shift() || '');
                }
            }
            this._settings.searchPatterns.forEach((p: RegExp) => {
                pattern = new RegExp(p.source, "g");
                let match = pattern.exec(line);
                while (match) {
                    if ((self._settings.linesBefore === 0 || self.linesBeforeMatch(linesBefore)) &&
                        (self._settings.linesAfter === 0 || self.linesAfterMatch(linesAfter))) {

                        results.push(new SearchResult(
                            pattern,
                            null,
                            lineNum,
                            match.index+1,
                            pattern.lastIndex+1,
                            line,
                            [...linesBefore],
                            [...linesAfter]));
                        if (self._settings.firstMatch) {
                            patternResults[pattern.source] = 1;
                            break;
                        }
                    }
                    match = pattern.exec(line);
                }
            });
            if (this._settings.linesBefore > 0) {
                if (linesBefore.length == this._settings.linesBefore)
                    linesBefore.shift();
                if (linesBefore.length < this._settings.linesBefore)
                    linesBefore.push(line);
            }
        }
        return results;
    }

    cmpSearchResults(r1: SearchResult, r2: SearchResult): number {
        let pathCmp = 0;
        if (r1.file && r2.file)
            pathCmp = r1.file.path.localeCompare(r2.file.path);
        if (pathCmp === 0) {
            let fileCmp = 0;
            if (r1.file && r2.file)
                fileCmp = r1.file.fileName.localeCompare(r2.file.fileName);
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

    printSearchResults(results: SearchResult[], formatter: SearchResultFormatter): void {
        // first sort the results
        // TODO: ensure sorting gets help from tsfind
        results.sort(this.cmpSearchResults);
        common.log("\nSearch results " + `(${results.length}):`);
        results.forEach(r => common.log(formatter.format(r)));
    }

    getFileResults(results: SearchResult[]): FileResult[] {
        let fileMap: {[key: string]: FileResult} = {};
        let fileResults: FileResult[] = [];
        for (const r of results) {
            if (r.file && !(r.file.relativePath() in fileMap)) {
                fileMap[r.file.relativePath()] = r.file;
                fileResults.push(r.file);
            }
        }
        return fileResults;
    }

    printMatchingDirs(results: SearchResult[], formatter: SearchResultFormatter): void {
        const fileResults: FileResult[] = this.getFileResults(results);
        this._finder.printMatchingDirs(fileResults, formatter.fileFormatter);
    }

    printMatchingFiles(results: SearchResult[], formatter: SearchResultFormatter): void {
        const fileResults: FileResult[] = this.getFileResults(results);
        this._finder.printMatchingFiles(fileResults, formatter.fileFormatter);
    }

    getMatchingLines(results: SearchResult[]): string[] {
        let lines: string[] = results.filter(r => r.lineNum > 0).map(r => r.line.trim());
        if (this._settings.uniqueLines) {
            lines = common.setFromArray(lines);
        }
        lines.sort((a, b) => {
            if (a.toUpperCase() === b.toUpperCase())
                return 0;
            return a.toUpperCase() < b.toUpperCase() ? -1 : 1;
        });
        return lines;
    }

    printMatchingLines(results: SearchResult[], formatter: SearchResultFormatter): void {
        const lines: string[] = this.getMatchingLines(results);
        let hdrText: string;
        if (this._settings.uniqueLines)
            hdrText = "\nUnique lines with matches";
        else
            hdrText = "\nLines with matches";
        if (lines.length > 0) {
            hdrText = `${hdrText} (${lines.length}):`
            common.log(hdrText);
            lines.forEach(l => common.log(formatter.formatLine(l)));
        } else {
            hdrText = `${hdrText}: 0`
            common.log(hdrText);
        }
    }
}
