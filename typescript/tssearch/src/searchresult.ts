/// <reference path="../typings/node/node.d.ts"/>
/*
 * searchresults.ts
 *
 * SearchResult class represents a search result
 */

"use strict";

class SearchResult {
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
        var s = this.filename;
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
        var maxLineNum: number = this.linenum + this.linesAfter.length;
        return ("" + maxLineNum).length;
    }

    private static padLeft(s: string, i: number): string {
        var p: string = "" + s;
        while (p.length < i) {
            p = " " + p;
        }
        return p;
    }

    private static trimRight(s: string): string {
        return s.replace(/[\r\n]+$/, '');
    }

    private multiLineToString(): string {
        var s: string = Array(81).join("=") + "\n" + this.filename + ": " +
            this.linenum + ": [" + this.matchStartIndex + ":" +
            this.matchEndIndex + "]\n" + Array(81).join("-") + "\n";
        var currentLineNum: number = this.linenum;
        var numPadding = this.lineNumPadding();
        if (this.linesBefore.length > 0) {
            currentLineNum = currentLineNum - this.linesBefore.length;
            for (var i: number = 0; i < this.linesBefore.length; i++) {
                var b = SearchResult.trimRight(this.linesBefore[i]);
                s += "  " + SearchResult.padLeft(currentLineNum.toString(), numPadding) + " | " + b + "\n";
                currentLineNum++;
            }
        }
        s += "> " + SearchResult.padLeft(currentLineNum.toString(), numPadding) + " | " +
            SearchResult.trimRight(this.line) + "\n";
        if (this.linesAfter.length > 0) {
            currentLineNum++;
            for (var i: number = 0; i < this.linesAfter.length; i++) {
                var a = SearchResult.trimRight(this.linesAfter[i]);
                s += "  " + SearchResult.padLeft(currentLineNum.toString(), numPadding) + " | " + a + "\n";
                currentLineNum++;
            }
        }
        return s;
    }

    private formatMatchingLine(): string {
        var formatted: string = this.line;
        var lineLength: number = this.line.length;
        var matchLength: number = this.matchEndIndex - this.matchStartIndex;
        if (lineLength > this.maxLineLength) {
            var adjustedMaxLength: number = this.maxLineLength - matchLength;
            var beforeIndex: number = this.matchStartIndex;
            if (this.matchStartIndex > 0) {
                beforeIndex = beforeIndex - (adjustedMaxLength / 4);
                if (beforeIndex < 0)
                    beforeIndex = 0;
            }
            adjustedMaxLength = adjustedMaxLength - (this.matchStartIndex - beforeIndex);
            var afterIndex = this.matchEndIndex + adjustedMaxLength;
            if (afterIndex > lineLength)
                afterIndex = lineLength;

            var before: string = '';
            if (beforeIndex > 3) {
                before = '...';
                beforeIndex += 3;
            }
            var after: string = '';
            if (afterIndex < lineLength - 3) {
                after = '...';
                afterIndex -= 3;
            }
            formatted = before + this.line.substring(beforeIndex, afterIndex) + after;
        }
        return formatted.trim();
    }
}

exports.SearchResult = SearchResult;
