/*
 * searchresultformatter.js
 *
 * SearchResultFormatter class provides formatting of search result instances
 */
const {COLORS} = require('./common');

const SEPARATOR_LEN = 80;

class SearchResultFormatter {
    'use strict'

    constructor(settings) {
        this.settings = settings;
    }

    format(result) {
        return result.linesBefore.length + result.linesAfter.length > 0 ?
            this.multiLineFormat(result) : this.singleLineFormat(result);
    }

    singleLineFormat(result) {
        let s = result.file ? result.file.toString() : '<text>';
        if (result.linenum && result.line) {
            s += `: ${result.linenum}: [${result.matchStartIndex}:` +
                `${result.matchEndIndex}]: ` + this.formatMatchingLine(result);
        } else {
            s += ` matches at [${result.matchStartIndex}:${result.matchEndIndex}]`;
        }
        return s;
    }

    lineNumPadding(result) {
        let maxLineNum = result.linenum + result.linesAfter.length;
        return ("" + maxLineNum).length;
    }

    padLeft(s, i) {
        let p = "" + s;
        while (p.length < i) {
            p = ' '.concat(p);
        }
        return p;
    }

    trimRight(s) {
        return s.replace(/[\r\n]+$/, '');
    }

    colorize(s, matchStartIndex, matchEndIndex) {
        return s.slice(0, matchStartIndex) +
            COLORS.GREEN +
            s.slice(matchStartIndex, matchEndIndex) +
            COLORS.RESET +
            s.slice(matchEndIndex);
    }

    multiLineFormat(result) {
        const filename = result.file ? result.file.toString() : '<text>';
        let s = new Array(SEPARATOR_LEN + 1).join("=") + "\n" + `${filename}: ` +
            `${result.linenum}: [${result.matchStartIndex}:${result.matchEndIndex}]` +
            "\n" + new Array(SEPARATOR_LEN + 1).join("-") + "\n";
        let currentLineNum = result.linenum;
        let numPadding = this.lineNumPadding(result);
        if (result.linesBefore.length > 0) {
            currentLineNum = currentLineNum - result.linesBefore.length;
            result.linesBefore.forEach(lineBefore => {
                let b = this.trimRight(lineBefore);
                s += "  " + this.padLeft(currentLineNum, numPadding) + " | " + b + "\n";
                currentLineNum++;
            });
        }
        let line = this.trimRight(result.line);
        if (this.settings.colorize) {
            line = this.colorize(line, result.matchStartIndex - 1, result.matchEndIndex - 1);
        }
        s += "> " + this.padLeft(currentLineNum, numPadding) + " | " + line + "\n";
        if (result.linesAfter.length > 0) {
            currentLineNum++;
            result.linesAfter.forEach(lineAfter => {
                let a = this.trimRight(lineAfter);
                s += "  " + this.padLeft(currentLineNum, numPadding) + " | " + a + "\n";
                currentLineNum++;
            });
        }
        return s;
    }

    formatMatchingLine(result) {
        let formatted = this.trimRight(result.line);
        let leadingWhitespaceCount = 0;
        const whitespaceChars = [' ', '\t', '\n', '\r'];
        while (whitespaceChars.indexOf(formatted.charAt(leadingWhitespaceCount)) > -1) {
            leadingWhitespaceCount++;
        }
        formatted = formatted.trim();
        let formattedLength = formatted.length;
        let maxLineEndIndex = formattedLength - 1;
        let matchLength = result.matchEndIndex - result.matchStartIndex;

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
            formatted = this.colorize(formatted, matchStartIndex, matchEndIndex);
        }
        return formatted;
    }
}

exports.SearchResultFormatter = SearchResultFormatter;
