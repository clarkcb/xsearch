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
        var s = self.filename;
        if (self.linenum && self.line) {
            s = s + ': ' + self.linenum + ' [' + self.matchStartIndex + ':' +
                self.matchEndIndex +']: ' + formatMatchingLine();
        } else {
            s = s + ' matches';
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
