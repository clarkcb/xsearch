/*
 * nodesearch.js
 *
 * file search utility written in node.js
 */

function SearchResult(pattern, filename, linenum, line) {
    var that = this;
    this.pattern = pattern;
    this.filename = filename;
    this.linenum = linenum;
    this.line = line;

    this.toString = function () {
        var s = that.filename;
        if (that.linenum) {
            s = s + ': ' + that.linenum + ': ' + that.line.trim();
        } else {
            s = s + ' has match';
        }
        return s;
    };
}

exports.SearchResult = SearchResult;