/*
 * searchresults.ts
 *
 * SearchResult class represents a search result
 */

import {SearchFile} from './searchfile';

"use strict";

export class SearchResult {
   pattern: RegExp;
   file?: SearchFile;
   linenum: number;
   matchStartIndex: number;
   matchEndIndex: number;
   line: string;
   linesBefore: string[];
   linesAfter: string[];

    constructor(pattern: RegExp, linenum: number, matchStartIndex: number, matchEndIndex: number, line: string,
                linesBefore: string[], linesAfter: string[]) {
        this.pattern = pattern;
        this.linenum = linenum;
        this.matchStartIndex = matchStartIndex;
        this.matchEndIndex = matchEndIndex;
        this.line = line;
        this.linesBefore = linesBefore;
        this.linesAfter = linesAfter;
    }
}
