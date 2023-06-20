/*
 * searchresults.ts
 *
 * SearchResult class represents a search result
 */

import {COLORS} from './color';
import {SearchResult} from './searchresult';
import {SearchSettings} from './searchsettings';

"use strict";

const SEPARATOR_LEN = 80;

export class SearchResultFormatter {
    settings: SearchSettings;

    constructor(settings: SearchSettings) {
        this.settings = settings;
    }

    public format(result: SearchResult): string {
        return result.linesBefore.length + result.linesAfter.length > 0 ?
            this.multiLineFormat(result) : this.singleLineFormat(result);
    }

    private singleLineFormat(result: SearchResult): string {
        let s = result.file ? result.file.toString() : '<text>';
        if (result.lineNum && result.line) {
            s += ': ' + result.lineNum + ': [' + result.matchStartIndex + ':' +
                result.matchEndIndex +']: ' + this.formatMatchingLine(result);
        } else {
            s += ' matches at [' + result.matchStartIndex + ':' +
                result.matchEndIndex + ']';
        }
        return s;
    }

    private static lineNumPadding(result: SearchResult): number {
        const maxLineNum: number = result.lineNum + result.linesAfter.length;
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

    private static colorize(s: string, matchStartIndex: number, matchEndIndex: number): string {
        return s.slice(0, matchStartIndex) +
            COLORS.GREEN +
            s.slice(matchStartIndex, matchEndIndex) +
            COLORS.RESET +
            s.slice(matchEndIndex);
    }

    private multiLineFormat(result: SearchResult): string {
        const filename = result.file ? result.file.toString() : '<text>';
        let s: string = Array(SEPARATOR_LEN + 1).join("=") + "\n" + `${filename}: ` +
            `${result.lineNum}: [${result.matchStartIndex}:${result.matchEndIndex}]` +
             "\n" + Array(SEPARATOR_LEN + 1).join("-") + "\n";
        let currentLineNum: number = result.lineNum;
        const numPadding: number = SearchResultFormatter.lineNumPadding(result);
        if (result.linesBefore.length > 0) {
            currentLineNum = currentLineNum - result.linesBefore.length;
            for (let i = 0; i < result.linesBefore.length; i++) {
                const b = SearchResultFormatter.trimRight(result.linesBefore[i]);
                s += "  " + SearchResultFormatter.padLeft(currentLineNum.toString(), numPadding) +
                    " | " + b + "\n";
                currentLineNum++;
            }
        }
        let line = SearchResultFormatter.trimRight(result.line);
        if (this.settings.colorize) {
            line = SearchResultFormatter.colorize(line, result.matchStartIndex - 1, result.matchEndIndex - 1);
        }
        s += "> " + SearchResultFormatter.padLeft(currentLineNum.toString(), numPadding) + " | " +
            line + "\n";
        if (result.linesAfter.length > 0) {
            currentLineNum++;
            for (let i = 0; i < result.linesAfter.length; i++) {
                const a = SearchResultFormatter.trimRight(result.linesAfter[i]);
                s += "  " + SearchResultFormatter.padLeft(currentLineNum.toString(), numPadding) +
                    " | " + a + "\n";
                currentLineNum++;
            }
        }
        return s;
    }

    private formatMatchingLine(result: SearchResult): string {
        let formatted: string = SearchResultFormatter.trimRight(result.line);
        let leadingWhitespaceCount = 0;
        const whitespaceChars = [' ', '\t', '\n', '\r'];
        while (whitespaceChars.indexOf(formatted.charAt(leadingWhitespaceCount)) > -1) {
            leadingWhitespaceCount++;
        }
        formatted = formatted.trim();
        let formattedLength = formatted.length;
        const maxLineEndIndex = formattedLength - 1;
        const matchLength = result.matchEndIndex - result.matchStartIndex;

        // track where match start and end indices end up
        let matchStartIndex = result.matchStartIndex - 1 - leadingWhitespaceCount;
        let matchEndIndex = matchStartIndex + matchLength;

        // if longer than maxlinelength, walk out from matching indices
        if (formattedLength > this.settings.maxLineLength) {
            let lineStartIndex = matchStartIndex;
            let lineEndIndex = lineStartIndex + matchLength;
            matchStartIndex = 0;
            matchEndIndex = matchLength;

            while (lineEndIndex > formattedLength - 1) {
                lineStartIndex--;
                lineEndIndex--;
                matchStartIndex++;
                matchEndIndex++;
            }

            formattedLength = lineEndIndex - lineStartIndex;

            while (formattedLength < this.settings.maxLineLength) {
                if (lineStartIndex > 0) {
                    lineStartIndex--;
                    matchStartIndex++;
                    matchEndIndex++;
                    formattedLength = lineEndIndex - lineStartIndex;
                }
                if (formattedLength < this.settings.maxLineLength && lineEndIndex < maxLineEndIndex) {
                    lineEndIndex++;
                }
                formattedLength = lineEndIndex - lineStartIndex;
            }

            formatted = formatted.slice(lineStartIndex, lineEndIndex);

            if (lineStartIndex > 2) {
                formatted = '...' + formatted.slice(3);
            }
            if (lineEndIndex < maxLineEndIndex - 3) {
                formatted = formatted.slice(0, formatted.length - 3) + '...';
            }
        }

        if (this.settings.colorize) {
            formatted = SearchResultFormatter.colorize(formatted, matchStartIndex, matchEndIndex);
        }
        return formatted;
    }
}
