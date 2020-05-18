/*
 * searchresults.js
 *
 * SearchResult class represents a search result
 */

class SearchResult {
    "use strict";

    constructor(pattern, file, linenum, matchStartIndex, matchEndIndex, line, linesBefore, linesAfter) {
        this.pattern = pattern;
        // file is SearchFile instance
        this.file = file;
        this.linenum = linenum;
        this.matchStartIndex = matchStartIndex;
        this.matchEndIndex = matchEndIndex;
        this.line = line;
        this.linesBefore = linesBefore;
        this.linesAfter = linesAfter;
        this.maxLineLength = 150;
    }

    toString() {
        return this.linesBefore.length + this.linesAfter.length > 0 ?
            this.multiLineToString() : this.singleLineToString();
    }

    singleLineToString() {
        let s = this.file ? this.file.toString() : '<text>';
        if (this.linenum && this.line) {
            s += `: ${this.linenum}: [${this.matchStartIndex}:` +
                `${this.matchEndIndex}]: ` + this.formatMatchingLine();
        } else {
            s += ` matches at [${this.matchStartIndex}:${this.matchEndIndex}]`;
        }
        return s;
    }

    lineNumPadding() {
        let maxLineNum = this.linenum + this.linesAfter.length;
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

    multiLineToString() {
        const filename = this.file ? this.file.toString() : '<text>';
        let s = new Array(81).join("=") + "\n" + `${filename}: ` +
            `${this.linenum}: [${this.matchStartIndex}:${this.matchEndIndex}]` +
            "\n" + new Array(81).join("-") + "\n";
        let currentLineNum = this.linenum;
        let numPadding = this.lineNumPadding();
        if (this.linesBefore.length > 0) {
            currentLineNum = currentLineNum - this.linesBefore.length;
            this.linesBefore.forEach(lineBefore => {
                let b = this.trimRight(lineBefore);
                s += "  " + this.padLeft(currentLineNum, numPadding) + " | " + b + "\n";
                currentLineNum++;
            });
        }
        s += "> " + this.padLeft(currentLineNum, numPadding) + " | " +
            this.trimRight(this.line) + "\n";
        if (this.linesAfter.length > 0) {
            currentLineNum++;
            this.linesAfter.forEach(lineAfter => {
                let a = this.trimRight(lineAfter);
                s += "  " + this.padLeft(currentLineNum, numPadding) + " | " + a + "\n";
                currentLineNum++;
            });
        }
        return s;
    };

    formatMatchingLine() {
        let formatted = this.line;
        let lineLength = this.line.length;
        if (lineLength > this.maxLineLength) {
            let startIndex = this.matchStartIndex;
            let endIndex = this.matchEndIndex;
            let formattedLength = endIndex - startIndex;
            while (formattedLength < this.maxLineLength) {
                if (startIndex > 0)
                    startIndex--;
                if (endIndex < lineLength - 1)
                    endIndex++;
                formattedLength = endIndex - startIndex;
            }
            formatted = this.line.slice(startIndex, endIndex - 1);
            const whitespaceChars = [' ', '\t', '\n', '\r'];
            while (whitespaceChars.indexOf(formatted.charAt(0)) > -1) {
                formatted = formatted.slice(1);
                startIndex++;
            }
            while (whitespaceChars.indexOf(formatted.charAt(formatted.length - 1)) > -1) {
                formatted = formatted.slice(0, formatted.length - 1);
                endIndex--;
            }
            if (startIndex > 2) {
                formatted = '...' + formatted.slice(3);
            }
            if (endIndex < lineLength - 3) {
                formatted = formatted.slice(0, formatted.length - 3) + '...';
            }
        }
        return formatted.trim();
    };
}

exports.SearchResult = SearchResult;
