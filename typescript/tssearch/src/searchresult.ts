/*
 * searchresults.ts
 *
 * SearchResult class represents a search result
 */

import {SearchFile} from './searchfile';

"use strict";

export class SearchResult {
   pattern: RegExp;
   filename: string;
   linenum: number;
   matchStartIndex: number;
   matchEndIndex: number;
   line: string;
   linesBefore: string[];
   linesAfter: string[];
   maxLineLength: number;

    constructor(pattern: RegExp, filename: string, linenum: number,
                matchStartIndex: number, matchEndIndex: number, line: string,
                linesBefore: string[], linesAfter: string[]) {
        this.pattern = pattern;
        this.filename = filename;
        this.linenum = linenum;
        this.matchStartIndex = matchStartIndex;
        this.matchEndIndex = matchEndIndex;
        this.line = line;
        this.linesBefore = linesBefore;
        this.linesAfter = linesAfter;
        this.maxLineLength = 150;
    }

    public toString(): string {
        return this.linesBefore.length + this.linesAfter.length > 0 ?
            this.multiLineToString() : this.singleLineToString();
    }

    private singleLineToString(): string {
        let s = this.filename;
        if (this.linenum && this.line) {
            s += ': ' + this.linenum + ': [' + this.matchStartIndex + ':' +
                this.matchEndIndex +']: ' + this.formatMatchingLine();
        } else {
            s += ' matches at [' + this.matchStartIndex + ':' +
                this.matchEndIndex + ']';
        }
        return s;
    }

    private lineNumPadding(): number {
        let maxLineNum: number = this.linenum + this.linesAfter.length;
        return ("" + maxLineNum).length;
    }

    private static padLeft(s: string, i: number): string {
        let p: string = s.slice(0);
        while (p.length < i) {
            p = ' '.concat(p);
        }
        return p;
    }

    private static trimRight(s: string): string {
        return s.replace(/[\r\n]+$/, '');
    }

    private multiLineToString(): string {
        let s: string = Array(81).join("=") + "\n" + `${this.filename}: ` +
            `${this.linenum}: [${this.matchStartIndex}:${this.matchEndIndex}]` +
             "\n" + Array(81).join("-") + "\n";
        let currentLineNum: number = this.linenum;
        let numPadding: number = this.lineNumPadding();
        if (this.linesBefore.length > 0) {
            currentLineNum = currentLineNum - this.linesBefore.length;
            for (let i: number = 0; i < this.linesBefore.length; i++) {
                let b = SearchResult.trimRight(this.linesBefore[i]);
                s += "  " + SearchResult.padLeft(currentLineNum.toString(), numPadding) +
                    " | " + b + "\n";
                currentLineNum++;
            }
        }
        s += "> " + SearchResult.padLeft(currentLineNum.toString(), numPadding) + " | " +
            SearchResult.trimRight(this.line) + "\n";
        if (this.linesAfter.length > 0) {
            currentLineNum++;
            for (let i: number = 0; i < this.linesAfter.length; i++) {
                let a = SearchResult.trimRight(this.linesAfter[i]);
                s += "  " + SearchResult.padLeft(currentLineNum.toString(), numPadding) +
                    " | " + a + "\n";
                currentLineNum++;
            }
        }
        return s;
    }

    private formatMatchingLine(): string {
        let formatted: string = this.line.trim().slice(0);
        let lineLength: number = this.line.length;
        let matchLength: number = this.matchEndIndex - this.matchStartIndex;
        if (lineLength > this.maxLineLength) {
            let adjustedMaxLength: number = this.maxLineLength - matchLength;
            let beforeIndex: number = this.matchStartIndex;
            if (this.matchStartIndex > 0) {
                beforeIndex = beforeIndex - (adjustedMaxLength / 4);
                if (beforeIndex < 0)
                    beforeIndex = 0;
            }
            adjustedMaxLength = adjustedMaxLength - (this.matchStartIndex - beforeIndex);
            let afterIndex = this.matchEndIndex + adjustedMaxLength;
            if (afterIndex > lineLength)
                afterIndex = lineLength;

            let before: string = '';
            if (beforeIndex > 3) {
                before = '...';
                beforeIndex += 3;
            }
            let after: string = '';
            if (afterIndex < lineLength - 3) {
                after = '...';
                afterIndex -= 3;
            }
            formatted = before + this.line.substring(beforeIndex, afterIndex) + after;
        }
        return formatted;
    }
}
