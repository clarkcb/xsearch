/*
 * searchresult.ts
 *
 * SearchResult class represents a search result
 */

import {FileResult} from 'tsfind';

"use strict";

export class SearchResult {
   pattern: RegExp;
   file?: FileResult | null;
   lineNum: number;
   matchStartIndex: number;
   matchEndIndex: number;
   line: string;
   linesBefore: string[];
   linesAfter: string[];

    constructor(pattern: RegExp, file: FileResult | null, lineNum: number, matchStartIndex: number, matchEndIndex: number,
                line: string, linesBefore: string[], linesAfter: string[]) {
        this.pattern = pattern;
        this.file = file;
        this.lineNum = lineNum;
        this.matchStartIndex = matchStartIndex;
        this.matchEndIndex = matchEndIndex;
        this.line = line;
        this.linesBefore = linesBefore;
        this.linesAfter = linesAfter;
    }
}
