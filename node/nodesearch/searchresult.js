/*
 * searchresults.js
 *
 * SearchResult class represents a search result
 */

function SearchResult(pattern, filename, linenum, matchStartIndex, matchEndIndex,
    line, linesBefore, linesAfter) {
    var self = this;
    self.pattern = pattern;
    self.filename = filename;
    self.linenum = linenum;
    self.matchStartIndex = matchStartIndex;
    self.matchEndIndex = matchEndIndex;
    self.line = line;
    self.linesBefore = linesBefore;
    self.linesAfter = linesAfter;
    self.maxLineLength = 150;

    self.toString = function () {
        if (self.linesBefore.length > 0 || self.linesAfter.length > 0) {
            return multiLineToString();
        }
        return singleLineToString();
    };

    var singleLineToString = function () {
        var s = self.filename;
        if (self.linenum && self.line) {
            s += ': ' + self.linenum + ': [' + self.matchStartIndex + ':' +
                self.matchEndIndex +']: ' + formatMatchingLine();
        } else {
            s += ' matches';
        }
        return s;
    };

    var lineNumPadding = function() {
        var maxLineNum = self.linenum + self.linesAfter.length;
        return ("" + maxLineNum).length;
    }

    var padLeft = function(s, i) {
        p = "" + s;
        while (p.length < i) {
            p = " " + p;
        }
        return p;
    }

    var multiLineToString = function () {
        var s = Array(81).join("=") + "\n";
        s += "{0}: {1}: ".format(self.filename, self.linenum);
        s += "[{0}:{1}]\n".format(self.matchStartIndex, self.matchEndIndex);
        s += Array(81).join("-") + "\n";
        var currentLineNum = self.linenum;
        var numPadding = lineNumPadding();
        if (self.linesBefore.length > 0) {
            currentLineNum = currentLineNum - self.linesBefore.length;
            for (var i in self.linesBefore) {
                var b = self.linesBefore[i];
                s += "  " + padLeft(currentLineNum, numPadding) + " | " + b + "\n";
                currentLineNum++;
            }
        }
        s += "> " + padLeft(currentLineNum, numPadding) + " | " + self.line + "\n";
        if (self.linesAfter.length > 0) {
            currentLineNum++;
            for (var i in self.linesAfter) {
                var a = self.linesAfter[i];
                s += "  " + padLeft(currentLineNum, numPadding) + " | " + a + "\n";
                currentLineNum++;
            }
        }
        return s;
    };

    var formatMatchingLine = function () {
        var formatted = self.line;
        var lineLength = self.line.length;
        var matchLength = self.matchEndIndex - self.matchStartIndex;
        if (lineLength > self.maxLineLength) {
            var adjustedMaxLength = self.maxLineLength - matchLength;
            var beforeIndex = self.matchStartIndex;
            if (self.matchStartIndex > 0) {
                beforeIndex = beforeIndex - (adjustedMaxLength / 4);
                if (beforeIndex < 0)
                    beforeIndex = 0;
            }
            adjustedMaxLength = adjustedMaxLength - (self.matchStartIndex - beforeIndex);
            var afterIndex = self.matchEndIndex + adjustedMaxLength
            if (afterIndex > lineLength)
                afterIndex = lineLength;

            before = '';
            if (beforeIndex > 3) {
                before = '...';
                beforeIndex += 3;
            }
            after = '';
            if (afterIndex < lineLength - 3) {
                after = '...';
                afterIndex -= 3;
            }
            formatted = before + self.line.substring(beforeIndex, afterIndex) + after;
        }
        return formatted.trim();
    };
}

exports.SearchResult = SearchResult;
