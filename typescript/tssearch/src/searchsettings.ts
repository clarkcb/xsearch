/*
 * searchsettings.ts
 *
 * represents the settings to use when performing the search
 */

"use strict";

import {Color, FindSettings} from 'tsfind';

export class SearchSettings extends FindSettings {
    firstMatch = false;
    inLinesAfterPatterns: RegExp[] = [];
    inLinesBeforePatterns: RegExp[] = [];
    lineColor = Color.GREEN;
    linesAfter = 0;
    linesAfterToPatterns: RegExp[] = [];
    linesAfterUntilPatterns: RegExp[] = [];
    linesBefore = 0;
    maxLineLength = 150;
    multilineSearch = false;
    outLinesAfterPatterns: RegExp[] = [];
    outLinesBeforePatterns: RegExp[] = [];
    printLines = false;
    printMatches = false;
    printResults = false;
    searchArchives = false;
    searchPatterns: RegExp[] = [];
    #textFileEncoding: BufferEncoding = "utf-8";
    uniqueLines = false;

    constructor() {
        super();
    }

    public addInLinesAfterPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.inLinesAfterPatterns);
    }

    public addInLinesBeforePatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.inLinesBeforePatterns);
    }

    public addLinesAfterToPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.linesAfterToPatterns);
    }

    public addLinesAfterUntilPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.linesAfterUntilPatterns);
    }

    public addOutLinesAfterPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.outLinesAfterPatterns);
    }

    public addOutLinesBeforePatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.outLinesBeforePatterns);
    }

    public addSearchPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.searchPatterns);
    }

    get textFileEncoding(): BufferEncoding {
        return this.#textFileEncoding;
    }

    set textFileEncoding(value: string) {
        this.#textFileEncoding = value as BufferEncoding;
    }
}
