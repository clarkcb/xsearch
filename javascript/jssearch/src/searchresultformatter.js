/*
 * searchresultformatter.js
 *
 * SearchResultFormatter class provides formatting of search result instances
 */
const {FileResultFormatter} = require('jsfind');

const SEPARATOR_LEN = 80;

class SearchResultFormatter {
    'use strict'

    constructor(settings) {
        this.settings = settings;
        this.fileFormatter = new FileResultFormatter(settings);
        if (settings.colorize) {
            this.formatLine = function(line) {
                return this.formatLineWithColor(line);
            };
            this.formatMatch = function(m) {
                return this.formatMatchWithColor(m);
            };
        }
    }

    formatLineWithColor(line) {
        let formattedLine = line;
        for (let p of this.settings.searchPatterns) {
            let m = p.exec(formattedLine);
            if (m) {
                formattedLine = this.fileFormatter.colorize(formattedLine, m.index, m.index + m[0].length, this.settings.lineColor);
                break;
            }
        }
        return formattedLine;
    }

    formatLine(line) {
        return line;
    }

    formatMatchWithColor(m) {
        return this.fileFormatter.colorize(m, 0, m.length, this.settings.lineColor);
    }

    formatMatch(m) {
        return m;
    }

    format(result) {
        return result.linesBefore.length + result.linesAfter.length > 0 ?
            this.multiLineFormat(result) : this.singleLineFormat(result);
    }

    singleLineFormat(result) {
        let s = result.file ? this.fileFormatter.formatFileResult(result.file) : '<text>';
        if (result.lineNum && result.line) {
            s += `: ${result.lineNum}: [${result.matchStartIndex}:` +
                `${result.matchEndIndex}]: ` + this.formatResultLine(result);
        } else {
            s += ` matches at [${result.matchStartIndex}:${result.matchEndIndex}]`;
        }
        return s;
    }

    lineNumPadding(result) {
        let maxLineNum = result.lineNum + result.linesAfter.length;
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

    multiLineFormat(result) {
        const filename = result.file ? this.fileFormatter.formatFileResult(result.file) : '<text>';
        let s = new Array(SEPARATOR_LEN + 1).join("=") + "\n" + `${filename}: ` +
            `${result.lineNum}: [${result.matchStartIndex}:${result.matchEndIndex}]` +
            "\n" + new Array(SEPARATOR_LEN + 1).join("-") + "\n";
        let currentLineNum = result.lineNum;
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
            line = this.fileFormatter.colorize(line, result.matchStartIndex - 1, result.matchEndIndex - 1);
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

    formatResultMatch(result) {
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

    formatResultLine(result) {
        if (!result.line || result.line.trim().length === 0 || this.settings.maxLineLength === 0) {
            return '';
        }

        let maxLimit = this.settings.maxLineLength > 0;

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

        let trimmedLength = lineEndIndex - lineStartIndex;

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

exports.SearchResultFormatter = SearchResultFormatter;
