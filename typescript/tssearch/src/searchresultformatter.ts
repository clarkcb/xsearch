/*
 * searchresults.ts
 *
 * SearchResult class represents a search result
 */

import {FileResultFormatter} from 'tsfind';
import {SearchResult} from './searchresult';
import {SearchSettings} from './searchsettings';

"use strict";

const SEPARATOR_LEN = 80;

export class SearchResultFormatter {
    settings: SearchSettings;
    fileFormatter: FileResultFormatter;

    constructor(settings: SearchSettings) {
        this.settings = settings;
        this.fileFormatter = new FileResultFormatter(settings.getFindSettings());
        if (settings.colorize) {
            this.formatLine = this.formatLineWithColor.bind(this);
            this.formatMatch = this.formatMatchWithColor.bind(this);
        }
    }

    private formatLineWithColor(line: string): string {
        let formattedLine = line;
        for (const p of this.settings.searchPatterns) {
            const m = p.exec(formattedLine);
            if (m) {
                formattedLine = this.fileFormatter.colorize(formattedLine, m.index, m.index + m[0].length,
                    this.settings.lineColor);
                break;
            }
        }
        return formattedLine;
    }

    public formatLine(line: string): string {
        return line;
    }

    private formatMatchWithColor(m: string): string {
        return this.fileFormatter.colorize(m, 0, m.length, this.settings.lineColor);
    }

    public formatMatch(m: string): string {
        return m;
    }

    public format(result: SearchResult): string {
        return result.linesBefore.length + result.linesAfter.length > 0 ?
            this.multiLineFormat(result) : this.singleLineFormat(result);
    }

    private singleLineFormat(result: SearchResult): string {
        let s = result.file ? this.fileFormatter.formatFileResult(result.file) : '<text>';
        if (result.lineNum && result.line) {
            s += ': ' + result.lineNum + ': [' + result.matchStartIndex + ':' +
                result.matchEndIndex +']: ' + this.formatResultLine(result);
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

    private multiLineFormat(result: SearchResult): string {
        const filename = result.file ? this.fileFormatter.formatFileResult(result.file) : '<text>';
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
            line = this.fileFormatter.colorize(line, result.matchStartIndex - 1, result.matchEndIndex - 1,
                this.settings.lineColor);
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

    private formatResultMatch(result: SearchResult): string {
        if (!result.line || result.line.trim().length === 0 || this.settings.maxLineLength === 0) {
            return '';
        }

        let matchStartIndex = result.matchStartIndex - 1;
        let matchEndIndex = result.matchEndIndex - 1;
        const matchLength = result.matchEndIndex - result.matchStartIndex;

        let prefix = '';
        let suffix = '';
        let colorStartIndex = 0;
        let colorEndIndex = matchLength;

        if (matchLength > this.settings.maxLineLength) {
            if (matchStartIndex > 2) {
                prefix = '...';
            }
            suffix = '...';
            colorStartIndex = prefix.length;
            colorEndIndex = this.settings.maxLineLength - suffix.length;
            matchEndIndex = matchStartIndex + colorEndIndex;
            matchStartIndex = matchStartIndex + colorStartIndex;
        }

        let matchString = prefix + result.line.slice(matchStartIndex, matchEndIndex) + suffix;

        if (this.settings.colorize) {
            matchString = this.fileFormatter.colorize(matchString, colorStartIndex, colorEndIndex, this.settings.lineColor);
        }

        return matchString;
    }

    private formatResultLine(result: SearchResult): string {
        if (!result.line || result.line.trim().length === 0 || this.settings.maxLineLength === 0) {
            return '';
        }

        const maxLimit = this.settings.maxLineLength > 0;

        if (maxLimit && (result.matchEndIndex - result.matchStartIndex) > this.settings.maxLineLength) {
            return this.formatResultMatch(result);
        }

        let lineStartIndex = 0;
        let lineEndIndex = result.line.length - 1;

        const whitespaceChars = [' ', '\t', '\n', '\r'];
        while (whitespaceChars.indexOf(result.line.charAt(lineStartIndex)) > -1) {
            lineStartIndex++;
        }
        while (whitespaceChars.indexOf(result.line.charAt(lineEndIndex)) > -1) {
            lineEndIndex--;
        }

        const matchLength = result.matchEndIndex - result.matchStartIndex;
        let matchStartIndex = result.matchStartIndex - 1 - lineStartIndex;
        let matchEndIndex = matchStartIndex + matchLength;

        let prefix = '';
        let suffix = '';

        const trimmedLength = lineEndIndex - lineStartIndex;

        if (maxLimit && trimmedLength > this.settings.maxLineLength) {
            lineStartIndex = result.matchStartIndex - 1;
            lineEndIndex = lineStartIndex + matchLength;
            matchStartIndex = 0;
            matchEndIndex = matchLength;

            let currentLen = lineEndIndex - lineStartIndex;
            while (currentLen < this.settings.maxLineLength) {
                if (lineStartIndex > 0) {
                    lineStartIndex--;
                    matchStartIndex++;
                    matchEndIndex++;
                    currentLen++;
                }
                if (currentLen < this.settings.maxLineLength && lineEndIndex < trimmedLength) {
                    lineEndIndex++;
                    currentLen++;
                }
            }

            if (lineStartIndex > 2) {
                prefix = '...';
                lineStartIndex += 3;
            }
            if (lineEndIndex < trimmedLength - 3) {
                suffix = '...';
                lineEndIndex -= 3;
            }
        } else {
            lineEndIndex++;
        }

        let formatted = prefix + result.line.slice(lineStartIndex, lineEndIndex) + suffix;

        if (this.settings.colorize) {
            formatted = this.fileFormatter.colorize(formatted, matchStartIndex, matchEndIndex, this.settings.lineColor);
        }

        return formatted;
    }
}
