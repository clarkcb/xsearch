/*
 * searchresults.js
 *
 * SearchResult class represents a search result
 */

class SearchResult {
    'use strict'

    constructor(pattern, file, lineNum, matchStartIndex, matchEndIndex, line, linesBefore, linesAfter) {
        this.pattern = pattern;
        // file is FileResult instance (jsfind)
        this.file = file;
        this.lineNum = lineNum;
        this.matchStartIndex = matchStartIndex;
        this.matchEndIndex = matchEndIndex;
        this.line = line;
        this.linesBefore = linesBefore;
        this.linesAfter = linesAfter;
    }
}

exports.SearchResult = SearchResult;
