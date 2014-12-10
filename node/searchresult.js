/*
 * nodesearch.js
 *
 * file search utility written in node.js
 */

function SearchResult(pattern, filename, linenum, line, matchStartIndex, matchEndIndex) {
    var that = this;
    this.pattern = pattern;
    this.filename = filename;
    this.linenum = linenum;
    this.line = line;
    this.matchStartIndex = matchStartIndex;
    this.matchEndIndex = matchEndIndex;
    this.maxLineLength = 150;

    this.toString = function () {
        var s = that.filename;
        if (that.linenum && that.line) {
            //s = s + ': ' + that.linenum + ': ' + that.line.trim();
            s = s + ': ' + that.linenum + ' [' + that.matchStartIndex + ':' +
                that.matchEndIndex +']: ' + formatMatchingLine();
        } else {
            s = s + ' matches';
        }
        return s;
    };

    var formatMatchingLine = function () {
        var formatted = that.line;
        var lineLength = that.line.length;
        var matchLength = that.matchEndIndex - that.matchStartIndex;
        if (lineLength > that.maxLineLength) {
            var adjustedMaxLength = that.maxLineLength - matchLength;
            var beforeIndex = that.matchStartIndex;
            if (that.matchStartIndex > 0) {
                beforeIndex = beforeIndex - (adjustedMaxLength / 4);
                if (beforeIndex < 0)
                    beforeIndex = 0;
            }
            adjustedMaxLength = adjustedMaxLength - (that.matchStartIndex - beforeIndex);
            var afterIndex = that.matchEndIndex + adjustedMaxLength
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
            formatted = before + that.line.substring(beforeIndex, afterIndex) + after;
        }
        return formatted.trim();
    };

}

exports.SearchResult = SearchResult;
