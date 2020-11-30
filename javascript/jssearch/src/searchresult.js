/*
 * searchresults.js
 *
 * SearchResult class represents a search result
 */

class SearchResult {
    'use strict'

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
    }
}

exports.SearchResult = SearchResult;
