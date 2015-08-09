/// <reference path="../typings/node/node.d.ts"/>
/// <reference path="common.ts"/>
/// <reference path="filetype.ts"/>
/// <reference path="filetypes.ts"/>
/// <reference path="fileutil.ts"/>
/// <reference path="searchresult.ts"/>
/// <reference path="searchsettings.ts"/>
/*
 * searcher.js
 *
 * performs the searching based on the given SearchSettings instance
 */

"use strict";

var assert = require('assert');
var fs = require('fs');
var path = require('path');

var common = require('./common.js');
var FileType = require('./filetype.js').FileType;
var FileTypes = require('./filetypes.js').FileTypes;
var FileUtil = require('./fileutil.js').FileUtil;
var SearchResult = require('./searchresult.js').SearchResult;
var SearchSettings = require('./searchresult.js').SearchSettings;

class Searcher {
    _settings: SearchSettings;
    results: SearchResult[] = [];

    constructor(settings: SearchSettings) {
        this._settings = settings;
        this.validateSettings();
    }

    private validateSettings(): void {
        assert.ok(this._settings.startPath, 'Startpath not defined');
        assert.ok(fs.existsSync(this._settings.startPath), 'Startpath not found');
        assert.ok(this.isSearchDir(this._settings.startPath), 'Startpath does not match search settings');
        assert.ok(this._settings.searchPatterns.length, 'No search patterns defined');
    }

    private static matchesAnyElement(s: string, elements: string[]): boolean {
        return elements.indexOf(s) > -1;
    }

    private static matchesAnyPattern(s: string, patterns: RegExp[]): boolean {
        for (var i: number = 0; i < patterns.length; i++) {
            var pattern = patterns[i];
            var match = pattern.exec(s);
            if (match) {
                return true;
            }
        }
        return false;
    }

    private static anyMatchesAnyPattern(ss: string[], patterns: RegExp[]) {
        for (var i: number = 0; i < ss.length; i++) {
            if (Searcher.matchesAnyPattern(ss[i], patterns)) {
                return true;
            }
        }
        return false;
    }

    public isSearchDir(dir: string): boolean {
        if (FileUtil.isDotDir(dir)) {
            return true;
        }
        var pathElems = dir.split(path.sep);
        if (this._settings.excludeHidden) {
            for (var i: number = 0; i < pathElems.length; i++) {
                if (!Searcher.matchesAnyElement(pathElems[i], ['.','..']) &&
                    FileUtil.isHidden(pathElems[i])) {
                    return false;
                }
            }
        }
        if (this._settings.inDirPatterns.length && !Searcher.matchesAnyPattern(dir,
                this._settings.inDirPatterns)) {
            return false;
        }
        return !(this._settings.outDirPatterns.length && Searcher.matchesAnyPattern(dir,
            this._settings.outDirPatterns));

    }

    public isSearchFile(file: string): boolean {
        if (FileUtil.isHidden(file) && this._settings.excludeHidden) {
            return false;
        }
        var ext = FileUtil.getExtension(file);
        if (this._settings.inExtensions.length &&
            !Searcher.matchesAnyElement(ext, this._settings.inExtensions)) {
            return false;
        }
        if (this._settings.outExtensions.length &&
            Searcher.matchesAnyElement(ext, this._settings.outExtensions)) {
            return false;
        }
        if (this._settings.inFilePatterns.length &&
            !Searcher.matchesAnyPattern(file, this._settings.inFilePatterns)) {
            return false;
        }
        return !(this._settings.outFilePatterns.length &&
        Searcher.matchesAnyPattern(file, this._settings.outFilePatterns));

    }

    public isArchiveSearchFile(file: string): boolean {
        if (FileUtil.isHidden(file) && this._settings.excludeHidden) {
            return false;
        }
        var ext = FileUtil.getExtension(file);
        if (this._settings.inArchiveExtensions.length &&
            !Searcher.matchesAnyElement(ext, this._settings.inArchiveExtensions)) {
            return false;
        }
        if (this._settings.outArchiveExtensions.length &&
            Searcher.matchesAnyElement(ext, this._settings.outArchiveExtensions)) {
            return false;
        }
        if (this._settings.inArchiveFilePatterns.length &&
            !Searcher.matchesAnyPattern(file, this._settings.inArchiveFilePatterns)) {
            return false;
        }
        return !(this._settings.outArchiveFilePatterns.length &&
        Searcher.matchesAnyPattern(file, this._settings.outArchiveFilePatterns));
    }

    private getSearchDirs(startPath: string): string[] {
        var searchDirs: string[] = [];
        var stats = fs.statSync(startPath);
        if (stats.isDirectory()) {
            if (this._settings.debug) {
                common.log("Getting list of directories to search under " + startPath);
            }
            searchDirs.push(startPath);
            if (this._settings.recursive) {
                searchDirs.push.apply(searchDirs, this.recGetSearchDirs(startPath));
            }
        } else if (stats.isFile()) {
            var d = path.dirname(startPath);
            if (!d) d = ".";
            searchDirs.push(d);
        }
        return searchDirs;
    }

    private static handleFsError(err): void {
        if (err.errno === 34 && err.code === "ENOENT") {
            // this error seems to occur when the file is a soft link
            // to a non-existent file
        } else {
            common.log(err);
            process.exit(1);
        }
    }

    private static getSubDirs(dir: string): string[] {
        var subDirs: string[] = [];
        var childItems = fs.readdirSync(dir);
        for (var i: number = 0; i < childItems.length; i++) {
            var filepath = path.join(dir, childItems[i]);
            try {
                var stats = fs.statSync(filepath);
                if (stats.isDirectory()) {
                    subDirs.push(filepath);
                }
            } catch (err) {
                Searcher.handleFsError(err);
            }
        }
        return subDirs;
    }

    private recGetSearchDirs(currentDir: string): string[] {
        var searchDirs: string[] = [];
        var subDirs: string[] = Searcher.getSubDirs(currentDir);
        for (var i: number = 0; i < subDirs.length; i++) {
            if (this.isSearchDir(subDirs[i])) {
                searchDirs.push(subDirs[i]);
            }
            searchDirs.push.apply(searchDirs, this.recGetSearchDirs(subDirs[i]));
        }
        return searchDirs;
    }

    private static getFilesForDirectory(dir: string): string[] {
        var files: string[] = [];
        var childItems = fs.readdirSync(dir);
        for (var i: number = 0; i < childItems.length; i++) {
            var filepath = path.join(dir, childItems[i]);
            try {
                var stats = fs.statSync(filepath);
                if (stats.isFile()) {
                    files.push(filepath);
                }
            } catch (err) {
                Searcher.handleFsError(err);
            }
        }
        return files;
    }

    public filterFile(f: string): boolean {
        if (FileTypes.isArchiveFile(f)) {
            return (this._settings.searchArchives && this.isArchiveSearchFile(f));
        }
        return (!this._settings.archivesOnly && this.isSearchFile(f));
    }

    private getSearchFilesForDirectory(dir: string): string[] {
        var searchFiles: string[] = [];
        var dirFiles = Searcher.getFilesForDirectory(dir);
        for (var i: number = 0; i < dirFiles.length; i++) {
            var f = dirFiles[i];
            if (this.filterFile(f)) {
                searchFiles.push(f);
            }
        }
        return searchFiles;
    }

    private getSearchFiles(searchDirs: string[]): string[] {
        var searchFiles: string[] = [];
        var stats = fs.statSync(this._settings.startPath);
        if (stats.isDirectory()) {
            for (var i: number = 0; i < searchDirs.length; i++) {
                searchFiles.push.apply(searchFiles, this.getSearchFilesForDirectory(searchDirs[i]));
            }
        } else if (stats.isFile()) {
            searchFiles.push(this._settings.startPath);
        }
        return searchFiles;
    }

    public search() {
        if (this._settings.verbose)
            common.log("Search initiated");

        // get the search dirs
        var dirs: string[] = [];
        dirs.push.apply(dirs, this.getSearchDirs(this._settings.startPath));
        if (this._settings.verbose) {
            common.logArray("\nDirectories to be searched ({0}):".format(dirs.length), dirs);
        }

        // get the search files
        var files: string[] = this.getSearchFiles(dirs);
        if (this._settings.verbose) {
            common.logArray("\nFiles to be searched ({0}):".format(files.length), files);
            common.log("");
        }

        for (var i: number = 0; i < files.length; i++) {
            this.searchFile(files[i]);
        }

        if (this._settings.verbose)
            common.log("Search complete.");
    }

    private searchFile(filepath: string): void {
        var filetype: FileType = FileTypes.getFileType(filepath);
        if (filetype === FileType.Text) {
            this.searchTextFile(filepath);
        } else if (filetype === FileType.Binary) {
            this.searchBinaryFile(filepath);
        }
    }

    private searchBinaryFile(filepath: string): void {
        if (this._settings.verbose) {
            common.log('Searching binary file: "{0}"'.format(filepath));
        }
        var contents = FileUtil.getFileContents(filepath);
        var pattern: RegExp;
        var patternResults = {};
        for (var i: number = 0; i < this._settings.searchPatterns.length; i++) {
            pattern = new RegExp(this._settings.searchPatterns[i].source, "g");
            if (this._settings.firstMatch && (pattern.source in patternResults)) {
                break;
            }
            var match = pattern.exec(contents);
            while (match) {
                this.addSearchResult(new SearchResult(
                    pattern,
                    filepath,
                    0,
                    match.index+1,
                    pattern.lastIndex+1,
                    null,
                    [],
                    []));
                if (this._settings.firstMatch) {
                    patternResults[pattern.source] = 1;
                    break;
                }
                match = pattern.exec(contents);
            }
        }
    }

    private searchTextFile(filepath: string): void {
        if (this._settings.verbose) {
            common.log('Searching text file {0}'.format(filepath));
        }
        if (this._settings.multilineSearch)
            this.searchTextFileContents(filepath);
        else
            this.searchTextFileLines(filepath);
    }

    private searchTextFileContents(filepath: string): void {
        var contents: string = FileUtil.getFileContents(filepath);
        var results: SearchResult[] = this.searchMultiLineString(contents);
        for (var i: number = 0; i < results.length; i++) {
            var r = results[i];
            var resultWithFilepath =
                new SearchResult(r.pattern, filepath, r.linenum,
                        r.matchStartIndex, r.matchEndIndex, r.line,
                        r.linesBefore, r.linesAfter);
            this.addSearchResult(resultWithFilepath);
        }
    }

    private static getNewLineIndices(s: string): number[] {
        var indices: number[] = [];
        for (var i = 0; i < s.length; i++) {
            if (s.charAt(i) == "\n") {
                indices.push(i);
            }
        }
        return indices;
    }

    private static getLinesAtIndices(s: string, atIndices: number[],
                                     startLineIndices: number[],
                                     endLineIndices: number[]) {
        if (atIndices.length === 0)
            return [];
        var lines: string[] = [];
        for (var a = 0; a < atIndices.length; a++) {
            var i = atIndices[a];
            var line = s.substring(i, endLineIndices[startLineIndices.indexOf(i)]);
            lines.push(line);
        }
        return lines;
    }

    private static getLinesBefore(s: string, beforeStartIndices: number[],
                                  startLineIndices: number[],
                                  endLineIndices: number[]) {
        return Searcher.getLinesAtIndices(s, beforeStartIndices,
            startLineIndices, endLineIndices);
    }

    private static getLinesAfter(s: string, afterStartIndices: number[],
                                 startLineIndices: number[],
                                 endLineIndices: number[]) {
        return Searcher.getLinesAtIndices(s, afterStartIndices,
            startLineIndices, endLineIndices);
    }

    private getLessThanOrEqual(matchIdx: number) {
        return function(i: number) { return i <= matchIdx; };
    }

    private getGreaterThan(matchIdx: number) {
        return function(i: number) { return i > matchIdx; };
    }

    public searchMultiLineString(s: string): SearchResult[] {
        var patternResults = {};
        var linesBefore: string[] = [];
        var linesAfter: string[] = [];
        var results: SearchResult[] = [];
        var newLineIndices: number[] = Searcher.getNewLineIndices(s);
        var plusOne = function(i: number) { return i+1; };
        var startLineIndices = [0].concat(newLineIndices.map(plusOne));
        var endLineIndices = newLineIndices.concat([s.length - 1]);
        for (var i: number = 0; i < this._settings.searchPatterns.length; i++) {
            var pattern: RegExp = new RegExp(this._settings.searchPatterns[i].source, "g");
            var match = pattern.exec(s);
            var stop: boolean = false;
            while (match && !stop) {
                if (this._settings.firstMatch && pattern.source in patternResults) {
                    stop = true;
                    continue;
                }
                var lessOrEqual = this.getLessThanOrEqual(match.index);
                var greaterThan = this.getGreaterThan(match.index);
                var lineStartIndex = 0;
                var lineEndIndex = s.length - 1;
                var beforeLineCount = 0;
                var beforeStartIndices = startLineIndices.filter(lessOrEqual);
                if (beforeStartIndices.length > 0) {
                    lineStartIndex = beforeStartIndices.pop();
                    beforeLineCount = beforeStartIndices.length;
                    if (beforeStartIndices.length > this._settings.linesBefore) {
                        beforeStartIndices = beforeStartIndices.slice(
                            beforeStartIndices.length - this._settings.linesBefore);
                    }
                }
                lineEndIndex = endLineIndices[startLineIndices.indexOf(lineStartIndex)];
                var line: string = s.substring(lineStartIndex, lineEndIndex);
                if (this._settings.linesBefore && beforeLineCount) {
                    linesBefore = Searcher.getLinesBefore(s, beforeStartIndices,
                        startLineIndices, endLineIndices);
                }
                if (this._settings.linesAfter) {
                    var afterStartIndices = startLineIndices.filter(greaterThan);
                    if (afterStartIndices.length > this._settings.linesAfter) {
                        afterStartIndices = afterStartIndices.slice(0,
                            this._settings.linesAfter);
                    }
                    linesAfter = Searcher.getLinesAfter(s, afterStartIndices,
                        startLineIndices, endLineIndices);
                }
                var matchStartIndex = match.index - lineStartIndex + 1;
                var matchEndIndex = pattern.lastIndex - lineStartIndex + 1;
                if ((this._settings.linesBefore === 0 || this.linesBeforeMatch(linesBefore)) &&
                    (this._settings.linesAfter === 0 || this.linesAfterMatch(linesAfter))) {
                    var searchResult = new SearchResult(
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
        }
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

    private searchTextFileLines(filepath: string): void {
        var lines = FileUtil.getFileLines(filepath);
        var rs: SearchResult[] = this.searchLines(lines);
        for (var i: number = 0; i < rs.length; i++) {
            var r = rs[i];
            var resultWithFilepath =
                new SearchResult(r.pattern, filepath, r.linenum,
                        r.matchStartIndex, r.matchEndIndex, r.line,
                        r.linesBefore, r.linesAfter);
            this.addSearchResult(resultWithFilepath);
        }
    }

    // return results so that filepath can be added to them
    public searchLines(lines: string[]): SearchResult[] {
        var linenum = 0;
        var pattern: RegExp;
        var linesBefore: string[] = [];
        var linesAfter: string[] = [];
        var results: SearchResult[] = [];
        var patternResults = {};
        while (true) {
            if (Object.keys(patternResults).length === this._settings.searchPatterns.length) {
                break;
            }
            var line = "";
            if (linesAfter.length > 0) {
                line = linesAfter.shift();
            } else if (lines.length > 0) {
                line = lines.shift();
            } else {
                break;
            }
            linenum += 1;
            if (this._settings.linesAfter > 0) {
                while (linesAfter.length < this._settings.linesAfter && lines.length > 0) {
                    linesAfter.push(lines.shift());
                }
            }
            for (var i: number = 0; i < this._settings.searchPatterns.length; i++) {
                pattern = new RegExp(this._settings.searchPatterns[i].source, "g");
                var match = pattern.exec(line);
                while (match) {
                    if ((this._settings.linesBefore === 0 || this.linesBeforeMatch(linesBefore)) &&
                        (this._settings.linesAfter === 0 || this.linesAfterMatch(linesAfter))) {
                        results.push(new SearchResult(
                            pattern,
                            '',
                            linenum,
                            match.index+1,
                            pattern.lastIndex+1,
                            line,
                            [].concat(linesBefore),
                            [].concat(linesAfter)));
                        if (this._settings.firstMatch) {
                            patternResults[pattern.source] = 1;
                            break;
                        }
                    }
                    match = pattern.exec(line);
                }
            }
            if (this._settings.linesBefore > 0) {
                if (linesBefore.length == this._settings.linesBefore)
                    linesBefore.shift();
                if (linesBefore.length < this._settings.linesBefore)
                    linesBefore.push(line);
            }
        }
        return results;
    }

    private addSearchResult(result: SearchResult): void {
        this.results.push(result);
    }

    private cmpSearchResults(r1: SearchResult, r2: SearchResult): number {
        var pathCmp = path.dirname(r1.filename).localeCompare(path.dirname(r2.filename));
        if (pathCmp === 0) {
            var fileCmp = path.basename(r1.filename).localeCompare(path.basename(r2.filename));
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

    public printSearchResults() {
        // first sort the results
        this.results.sort(this.cmpSearchResults);
        common.log("\nSearch results ({0}):".format(this.results.length));
        for (var i: number = 0; i < this.results.length; i++) {
            common.log(this.results[i].toString());
        }
    }

    public getMatchingDirs(): string[] {
        var dirs: string[] = [];
        for (var i: number = 0; i < this.results.length; i++) {
            var result = this.results[i];
            dirs.push(path.dirname(result.filename));
        }
        return common.setFromArray(dirs);
    }

    public printMatchingDirs(): void {
        var dirs: string[] = this.getMatchingDirs();
        common.logArray("\nDirectories with matches ({0}):".format(dirs.length), dirs);
    }

    public getMatchingFiles(): string[] {
        var files: string[] = [];
        for (var i: number = 0; i < this.results.length; i++) {
            var result = this.results[i];
            files.push(result.filename);
        }
        return common.setFromArray(files);
    }

    public printMatchingFiles(): void {
        var files: string[] = this.getMatchingFiles();
        common.logArray("\nFiles with matches ({0}):".format(files.length), files);
    }

    public getMatchingLines(): string[] {
        var lines: string[] = [];
        for (var i: number = 0; i < this.results.length; i++) {
            var result = this.results[i];
            if (result.linenum)
                lines.push(result.line.trim());
        }
        if (this._settings.uniqueLines)
            lines = common.setFromArray(lines);
        lines.sort(function (a, b) {
            if (a.toUpperCase() == b.toUpperCase())
                return 0;
            return a.toUpperCase() < b.toUpperCase() ? -1 : 1;
        });
        return lines;
    }

    public printMatchingLines() {
        var lines: string[] = this.getMatchingLines();
        var hdrText: string;
        if (this._settings.uniqueLines)
            hdrText = "\nUnique lines with matches ({0}):";
        else
            hdrText = "\nLines with matches ({0}):";
        common.logArray(hdrText.format(lines.length), lines);
    }
}

exports.Searcher = Searcher;