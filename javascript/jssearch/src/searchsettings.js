/*
 * searchsettings.js
 *
 * represents the settings to use when performing the search
 */

const {Color, FindSettings} = require('jsfind');

class SearchSettings extends FindSettings {
    firstMatch = false;
    inLinesAfterPatterns = [];
    inLinesBeforePatterns = [];
    lineColor = Color.GREEN;
    linesAfter = 0;
    linesAfterToPatterns = [];
    linesAfterUntilPatterns = [];
    linesBefore = 0;
    maxLineLength = 150;
    multilineSearch = false;
    outLinesAfterPatterns = [];
    outLinesBeforePatterns = [];
    printLines = false;
    printMatches = false;
    printResults = false;
    searchArchives = false;
    searchPatterns = [];
    textFileEncoding = "utf-8";
    uniqueLines = false;

    constructor() {
        super();
    }

    addInLinesAfterPatterns(pattern) {
        this.addPatterns(pattern, this.inLinesAfterPatterns);
    }

    addInLinesBeforePatterns(pattern) {
        this.addPatterns(pattern, this.inLinesBeforePatterns);
    }

    addLinesAfterToPatterns(pattern) {
        this.addPatterns(pattern, this.linesAfterToPatterns);
    }

    addLinesAfterUntilPatterns(pattern) {
        this.addPatterns(pattern, this.linesAfterUntilPatterns);
    }

    addOutLinesAfterPatterns(pattern) {
        this.addPatterns(pattern, this.outLinesAfterPatterns);
    }

    addOutLinesBeforePatterns(pattern) {
        this.addPatterns(pattern, this.outLinesBeforePatterns);
    }

    addSearchPatterns(pattern) {
        this.addPatterns(pattern, this.searchPatterns);
    }
}

exports.SearchSettings = SearchSettings;
