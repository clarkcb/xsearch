/// <reference path="../typings/node/node.d.ts"/>
/*
 * searchsettings.ts
 *
 * represents the settings to use when performing the search
 */

"use strict";

class SearchSettings {
    startPath: string = "";
    inExtensions: string[] = [];
    outExtensions: string[] = [];
    inDirPatterns: RegExp[] = [];
    outDirPatterns: RegExp[] = [];
    inFilePatterns: RegExp[] = [];
    outFilePatterns: RegExp[] = [];
    inArchiveExtensions: string[] = [];
    outArchiveExtensions: string[] = [];
    inArchiveFilePatterns: RegExp[] = [];
    outArchiveFilePatterns: RegExp[] = [];
    inLinesAfterPatterns: RegExp[] = [];
    outLinesAfterPatterns: RegExp[] = [];
    inLinesBeforePatterns: RegExp[] = [];
    outLinesBeforePatterns: RegExp[] = [];
    linesAfterToPatterns: RegExp[] = [];
    linesAfterUntilPatterns: RegExp[] = [];
    searchPatterns: RegExp[] = [];
    archivesOnly: boolean = false;
    debug: boolean = false;
    excludeHidden: boolean = true;
    firstMatch: boolean = false;
    linesAfter: number = 0;
    linesBefore: number = 0;
    listDirs: boolean = false;
    listFiles: boolean = false;
    listLines: boolean = false;
    maxLineLength: number = 150;
    multilineSearch: boolean = false;
    printResults: boolean = false;
    printUsage: boolean = false;
    printVersion: boolean = false;
    recursive: boolean = true;
    searchArchives: boolean = false;
    uniqueLines: boolean = false;
    verbose: boolean = false;

    private static addExtensions(exts: string, arr: string[]): void {
        var xs: string[] = exts.split(/,/);
        for (var i: number = 0; i < xs.length; i++) {
            if (xs[i] !== '')
                arr.push(xs[i]);
        }
    }

    public addInExtension(ext: string): void {
        SearchSettings.addExtensions(ext, this.inExtensions);
    }

    public addOutExtension(ext: string): void {
        SearchSettings.addExtensions(ext, this.outExtensions);
    }

    private static addPattern(pattern: string, arr: RegExp[]): void {
        arr.push(new RegExp(pattern));
    }

    public addInDirPattern(pattern: string): void {
        SearchSettings.addPattern(pattern, this.inDirPatterns);
    }

    public addOutDirPattern(pattern: string): void {
        SearchSettings.addPattern(pattern, this.outDirPatterns);
    }

    public addInFilePattern(pattern: string): void {
        SearchSettings.addPattern(pattern, this.inFilePatterns);
    }

    public addOutFilePattern(pattern: string): void {
        SearchSettings.addPattern(pattern, this.outFilePatterns);
    }

    public addSearchPattern(pattern: string): void {
        SearchSettings.addPattern(pattern, this.searchPatterns);
    }

    public addInArchiveExtension(ext: string): void {
        SearchSettings.addExtensions(ext, this.inArchiveExtensions);
    }

    public addOutArchiveExtension(ext: string): void {
        SearchSettings.addExtensions(ext, this.outArchiveExtensions);
    }

    public addInArchiveFilePattern(pattern: string): void {
        SearchSettings.addPattern(pattern, this.inArchiveFilePatterns);
    }
    public addOutArchiveFilePattern(pattern: string): void {
        SearchSettings.addPattern(pattern, this.outArchiveFilePatterns);
    }

    public addInLinesAfterPattern(pattern: string): void {
        SearchSettings.addPattern(pattern, this.inLinesAfterPatterns);
    }

    public addOutLinesAfterPattern(pattern: string): void {
        SearchSettings.addPattern(pattern, this.outLinesAfterPatterns);
    }

    public addInLinesBeforePattern(pattern: string): void {
        SearchSettings.addPattern(pattern, this.inLinesBeforePatterns);
    }

    public addOutLinesBeforePattern(pattern: string): void {
        SearchSettings.addPattern(pattern, this.outLinesBeforePatterns);
    }

    public addLinesAfterToPattern(pattern: string): void {
        SearchSettings.addPattern(pattern, this.linesAfterToPatterns);
    }

    public addLinesAfterUntilPattern(pattern: string): void {
        SearchSettings.addPattern(pattern, this.linesAfterUntilPatterns);
    }

    public setArchivesOnly(): void {
        this.archivesOnly = true;
        this.searchArchives = true;
    }

    public setDebug(): void {
        this.debug = true;
        this.verbose = true;
    }

    private static sListToString(name: string, lst: string[]): string {
        return name + '=["' + lst.join('","') + '"]';
    }

    private static rListToString(name: string, lst: RegExp[]): string {
        return name + '=["' + lst.join('","') + '"]';
    }

    public toString(): string {
        var s: string = 'SearchSettings('
            + 'archivesOnly=' + this.archivesOnly
            + ', debug=' + this.debug
            + ', excludeHidden=' + this.excludeHidden
            + ', firstMatch=' + this.firstMatch
            + ', ' + SearchSettings.sListToString('inArchiveExtensions', this.inArchiveExtensions)
            + ', ' + SearchSettings.rListToString('inArchiveFilePatterns', this.inArchiveFilePatterns)
            + ', ' + SearchSettings.rListToString('inDirPatterns', this.inDirPatterns)
            + ', ' + SearchSettings.sListToString('inExtensions', this.inExtensions)
            + ', ' + SearchSettings.rListToString('inFilePatterns', this.inFilePatterns)
            + ', ' + SearchSettings.rListToString('inLinesAfterPatterns', this.inLinesAfterPatterns)
            + ', ' + SearchSettings.rListToString('inLinesBeforePatterns', this.inLinesBeforePatterns)
            + ', linesAfter=' + this.linesAfter
            + ', ' + SearchSettings.rListToString('linesAfterToPatterns', this.linesAfterToPatterns)
            + ', ' + SearchSettings.rListToString('linesAfterUntilPatterns', this.linesAfterUntilPatterns)
            + ', linesBefore=' + this.linesBefore
            + ', listDirs=' + this.listDirs
            + ', listFiles=' + this.listFiles
            + ', listLines=' + this.listLines
            + ', maxLineLength=' + this.maxLineLength
            + ', multilineSearch=' + this.multilineSearch
            + ', ' + SearchSettings.sListToString('outArchiveExtensions', this.outArchiveExtensions)
            + ', ' + SearchSettings.rListToString('outArchiveFilePatterns', this.outArchiveFilePatterns)
            + ', ' + SearchSettings.rListToString('outDirPatterns', this.outDirPatterns)
            + ', ' + SearchSettings.sListToString('outExtensions', this.outExtensions)
            + ', ' + SearchSettings.rListToString('outFilePatterns', this.outFilePatterns)
            + ', ' + SearchSettings.rListToString('outLinesAfterPatterns', this.outLinesAfterPatterns)
            + ', ' + SearchSettings.rListToString('outLinesBeforePatterns', this.outLinesBeforePatterns)
            + ', printResults=' + this.printResults
            + ', printVersion=' + this.printVersion
            + ', recursive=' + this.recursive
            + ', searchArchives=' + this.searchArchives
            + ', ' + SearchSettings.rListToString('searchPatterns', this.searchPatterns)
            + ',  startPath="' + this.startPath + '"'
            + ', uniqueLines=' + this.uniqueLines
            + ', verbose=' + this.verbose
            + ')';
        return s;
    }
}

exports.SearchSettings = SearchSettings;
