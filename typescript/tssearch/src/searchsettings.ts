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
        exts.split(/,/).filter(x => x !== '').forEach(x => arr.push(x));
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

    private static listToString(name: string, lst: string[]|RegExp[]): string {
        return `${name}=["${lst.join('","')}"]`;
    }

    public toString(): string {
        return 'SearchSettings('
            + 'archivesOnly=' + this.archivesOnly
            + ', debug=' + this.debug
            + ', excludeHidden=' + this.excludeHidden
            + ', firstMatch=' + this.firstMatch
            + ', ' + SearchSettings.listToString('inArchiveExtensions', this.inArchiveExtensions)
            + ', ' + SearchSettings.listToString('inArchiveFilePatterns', this.inArchiveFilePatterns)
            + ', ' + SearchSettings.listToString('inDirPatterns', this.inDirPatterns)
            + ', ' + SearchSettings.listToString('inExtensions', this.inExtensions)
            + ', ' + SearchSettings.listToString('inFilePatterns', this.inFilePatterns)
            + ', ' + SearchSettings.listToString('inLinesAfterPatterns', this.inLinesAfterPatterns)
            + ', ' + SearchSettings.listToString('inLinesBeforePatterns', this.inLinesBeforePatterns)
            + ', linesAfter=' + this.linesAfter
            + ', ' + SearchSettings.listToString('linesAfterToPatterns', this.linesAfterToPatterns)
            + ', ' + SearchSettings.listToString('linesAfterUntilPatterns', this.linesAfterUntilPatterns)
            + ', linesBefore=' + this.linesBefore
            + ', listDirs=' + this.listDirs
            + ', listFiles=' + this.listFiles
            + ', listLines=' + this.listLines
            + ', maxLineLength=' + this.maxLineLength
            + ', multilineSearch=' + this.multilineSearch
            + ', ' + SearchSettings.listToString('outArchiveExtensions', this.outArchiveExtensions)
            + ', ' + SearchSettings.listToString('outArchiveFilePatterns', this.outArchiveFilePatterns)
            + ', ' + SearchSettings.listToString('outDirPatterns', this.outDirPatterns)
            + ', ' + SearchSettings.listToString('outExtensions', this.outExtensions)
            + ', ' + SearchSettings.listToString('outFilePatterns', this.outFilePatterns)
            + ', ' + SearchSettings.listToString('outLinesAfterPatterns', this.outLinesAfterPatterns)
            + ', ' + SearchSettings.listToString('outLinesBeforePatterns', this.outLinesBeforePatterns)
            + ', printResults=' + this.printResults
            + ', printVersion=' + this.printVersion
            + ', recursive=' + this.recursive
            + ', searchArchives=' + this.searchArchives
            + ', ' + SearchSettings.listToString('searchPatterns', this.searchPatterns)
            + ',  startPath="' + this.startPath + '"'
            + ', uniqueLines=' + this.uniqueLines
            + ', verbose=' + this.verbose
            + ')';
    }
}

exports.SearchSettings = SearchSettings;
