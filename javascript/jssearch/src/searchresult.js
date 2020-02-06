/*
 * searchresults.js
 *
 * SearchResult class represents a search result
 */

function SearchResult(pattern, file, linenum, matchStartIndex, matchEndIndex,
    line, linesBefore, linesAfter) {
    "use strict";
    let self = this;
    self.pattern = pattern;
    // file is SearchFile instance
    self.file = file;
    self.linenum = linenum;
    self.matchStartIndex = matchStartIndex;
    self.matchEndIndex = matchEndIndex;
    self.line = line;
    self.linesBefore = linesBefore;
    self.linesAfter = linesAfter;
    self.maxLineLength = 150;

    self.toString = function () {
        return self.linesBefore.length + self.linesAfter.length > 0 ?
            multiLineToString() : singleLineToString();
    };

    const singleLineToString = function () {
        let s = self.file.toString();
        if (self.linenum && self.line) {
            s += `: ${self.linenum}: [${self.matchStartIndex}:` +
                `${self.matchEndIndex}]: ` + formatMatchingLine();
        } else {
            s += ` matches at [${self.matchStartIndex}:${self.matchEndIndex}]`;
        }
        return s;
    };

    const lineNumPadding = function() {
        let maxLineNum = self.linenum + self.linesAfter.length;
        return ("" + maxLineNum).length;
    };

    const padLeft = function(s, i) {
        let p = "" + s;
        while (p.length < i) {
            p = ' '.concat(p);
        }
        return p;
    };

    const trimRight = function(s) {
        return s.replace(/[\r\n]+$/, '');
    };

    const multiLineToString = function () {
        let s = new Array(81).join("=") + "\n" + `${self.file}: ` +
            `${self.linenum}: [${self.matchStartIndex}:${self.matchEndIndex}]` +
            "\n" + new Array(81).join("-") + "\n";
        let currentLineNum = self.linenum;
        let numPadding = lineNumPadding();
        if (self.linesBefore.length > 0) {
            currentLineNum = currentLineNum - self.linesBefore.length;
            self.linesBefore.forEach(lineBefore => {
                let b = trimRight(lineBefore);
                s += "  " + padLeft(currentLineNum, numPadding) + " | " + b + "\n";
                currentLineNum++;
            });
        }
        s += "> " + padLeft(currentLineNum, numPadding) + " | " +
            trimRight(self.line) + "\n";
        if (self.linesAfter.length > 0) {
            currentLineNum++;
            self.linesAfter.forEach(lineAfter => {
                let a = trimRight(lineAfter);
                s += "  " + padLeft(currentLineNum, numPadding) + " | " + a + "\n";
                currentLineNum++;
            });


            for (let i = 0; i < self.linesAfter.length; i++) {
                let a = trimRight(self.linesAfter[i]);
                s += "  " + padLeft(currentLineNum, numPadding) + " | " + a + "\n";
                currentLineNum++;
            }
        }
        return s;
    };

    const formatMatchingLine = function () {
        let formatted = self.line.trim().slice(0);
        let lineLength = self.line.length;
        let matchLength = self.matchEndIndex - self.matchStartIndex;
        if (lineLength > self.maxLineLength) {
            let adjustedMaxLength = self.maxLineLength - matchLength;
            let beforeIndex = self.matchStartIndex;
            if (self.matchStartIndex > 0) {
                beforeIndex = beforeIndex - (adjustedMaxLength / 4);
                if (beforeIndex < 0)
                    beforeIndex = 0;
            }
            adjustedMaxLength = adjustedMaxLength - (self.matchStartIndex - beforeIndex);
            let afterIndex = self.matchEndIndex + adjustedMaxLength;
            if (afterIndex > lineLength)
                afterIndex = lineLength;

            let before = '';
            if (beforeIndex > 3) {
                before = '...';
                beforeIndex += 3;
            }
            let after = '';
            if (afterIndex < lineLength - 3) {
                after = '...';
                afterIndex -= 3;
            }
            formatted = before + self.line.substring(beforeIndex, afterIndex) + after;
        }
        return formatted;
    };
}

exports.SearchResult = SearchResult;
