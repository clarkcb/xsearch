/*
 * searchsettings.ts
 *
 * represents the settings to use when performing the search
 */

"use strict";

import {FileType} from './filetype';
import {FileTypes} from './filetypes';

export class SearchSettings {
    archivesOnly = false;
    colorize = true;
    debug = false;
    excludeHidden = true;
    firstMatch = false;
    inArchiveExtensions: string[] = [];
    inArchiveFilePatterns: RegExp[] = [];
    inDirPatterns: RegExp[] = [];
    inExtensions: string[] = [];
    inFilePatterns: RegExp[] = [];
    inFileTypes: FileType[] = [];
    inLinesAfterPatterns: RegExp[] = [];
    inLinesBeforePatterns: RegExp[] = [];
    linesAfter = 0;
    linesAfterToPatterns: RegExp[] = [];
    linesAfterUntilPatterns: RegExp[] = [];
    linesBefore = 0;
    listDirs = false;
    listFiles = false;
    listLines = false;
    maxLineLength = 150;
    multilineSearch = false;
    outArchiveExtensions: string[] = [];
    outArchiveFilePatterns: RegExp[] = [];
    outDirPatterns: RegExp[] = [];
    outExtensions: string[] = [];
    outFilePatterns: RegExp[] = [];
    outFileTypes: FileType[] = [];
    outLinesAfterPatterns: RegExp[] = [];
    outLinesBeforePatterns: RegExp[] = [];
    paths: string[] = [];
    printResults = false;
    printUsage = false;
    printVersion = false;
    recursive = true;
    searchArchives = false;
    searchPatterns: RegExp[] = [];
    textFileEncoding = "utf-8";
    uniqueLines = false;
    verbose = false;

    private static addExtensions(exts: string|string[], arr: string[]): void {
        if (typeof(exts) === 'string') {
            exts.split(/,/).filter(x => x !== '').forEach(x => arr.push(x));
        } else if (exts.constructor === Array) {
            exts.forEach((x: string) => arr.push(x));
        }
    }

    public addInExtensions(ext: string|string[]): void {
        SearchSettings.addExtensions(ext, this.inExtensions);
    }

    public addOutExtensions(ext: string|string[]): void {
        SearchSettings.addExtensions(ext, this.outExtensions);
    }

    private static addFileTypes(filetypes: string|string[], arr: FileType[]): void {
        if (typeof(filetypes) === 'string') {
            filetypes.split(/,/).filter(ft => ft !== '').
                forEach(ft => arr.push(FileTypes.fromName(ft)));
        } else if (filetypes.constructor === Array) {
            filetypes.forEach((ft: string) => arr.push(FileTypes.fromName(ft)));
        }
    }

    public addInFileTypes(filetype: string|string[]): void {
        SearchSettings.addFileTypes(filetype, this.inFileTypes);
    }

    public addOutFileTypes(filetype: string|string[]): void {
        SearchSettings.addFileTypes(filetype, this.outFileTypes);
    }

    private static addPatterns(patterns: string|string[], arr: RegExp[]): void {
        if (typeof(patterns) === 'string') {
            arr.push(new RegExp(patterns));
        } else if (patterns.constructor === Array) {
            patterns.forEach((p: string) => arr.push(new RegExp(p)));
        }
    }

    public addInDirPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.inDirPatterns);
    }

    public addOutDirPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.outDirPatterns);
    }

    public addInFilePatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.inFilePatterns);
    }

    public addOutFilePatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.outFilePatterns);
    }

    public addSearchPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.searchPatterns);
    }

    public addInArchiveExtensions(ext: string|string[]): void {
        SearchSettings.addExtensions(ext, this.inArchiveExtensions);
    }

    public addOutArchiveExtensions(ext: string|string[]): void {
        SearchSettings.addExtensions(ext, this.outArchiveExtensions);
    }

    public addInArchiveFilePatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.inArchiveFilePatterns);
    }
    public addOutArchiveFilePatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.outArchiveFilePatterns);
    }

    public addInLinesAfterPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.inLinesAfterPatterns);
    }

    public addOutLinesAfterPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.outLinesAfterPatterns);
    }

    public addInLinesBeforePatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.inLinesBeforePatterns);
    }

    public addOutLinesBeforePatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.outLinesBeforePatterns);
    }

    public addLinesAfterToPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.linesAfterToPatterns);
    }

    public addLinesAfterUntilPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.linesAfterUntilPatterns);
    }

    public setArchivesOnly(b: boolean): void {
        this.archivesOnly = b;
        if (b) this.searchArchives = b;
    }

    public setDebug(b: boolean): void {
        this.debug = b;
        if (b) this.verbose = b;
    }

    private static listToString(name: string, lst: string[]|RegExp[]): string {
        let s = `${name}=[`;
        if (lst.length)
            s += `"${lst.join('","')}"`;
        s += ']';
        return s;
    }

    private static fileTypesToString(name: string, fileTypes: FileType[]): string {
        let s = `${name}=[`;
        for (let i=0; i < fileTypes.length; i++) {
            if (i > 0) s += ', ';
            s += `"${FileTypes.toName(fileTypes[i])}"`;
        }
        s += ']';
        return s;
    }

    public toString(): string {
        return 'SearchSettings('
            + 'archivesOnly=' + this.archivesOnly
            + ', colorize=' + this.colorize
            + ', debug=' + this.debug
            + ', excludeHidden=' + this.excludeHidden
            + ', firstMatch=' + this.firstMatch
            + ', ' + SearchSettings.listToString('inArchiveExtensions', this.inArchiveExtensions)
            + ', ' + SearchSettings.listToString('inArchiveFilePatterns', this.inArchiveFilePatterns)
            + ', ' + SearchSettings.listToString('inDirPatterns', this.inDirPatterns)
            + ', ' + SearchSettings.listToString('inExtensions', this.inExtensions)
            + ', ' + SearchSettings.listToString('inFilePatterns', this.inFilePatterns)
            + ', ' + SearchSettings.fileTypesToString('inFileTypes', this.inFileTypes)
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
            + ', ' + SearchSettings.fileTypesToString('outFileTypes', this.outFileTypes)
            + ', ' + SearchSettings.listToString('outLinesAfterPatterns', this.outLinesAfterPatterns)
            + ', ' + SearchSettings.listToString('outLinesBeforePatterns', this.outLinesBeforePatterns)
            + ', ' + SearchSettings.listToString('paths', this.paths)
            + ', printResults=' + this.printResults
            + ', printVersion=' + this.printVersion
            + ', recursive=' + this.recursive
            + ', searchArchives=' + this.searchArchives
            + ', ' + SearchSettings.listToString('searchPatterns', this.searchPatterns)
            + ', textFileEncoding="' + this.textFileEncoding + '"'
            + ', uniqueLines=' + this.uniqueLines
            + ', verbose=' + this.verbose
            + ')';
    }
}
