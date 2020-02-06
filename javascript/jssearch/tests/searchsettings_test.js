/*
 * searchsettings_test.js
 *
 * Some nodeunit tests of searchsettings.js
 */

const SearchSettings = require('../src/searchsettings.js').SearchSettings;

exports.testDefaultSettings = (test) => {
    const settings = new SearchSettings();
    test.ok(!settings.archivesOnly, "archivesOnly is false by default");
    test.ok(!settings.debug, "debug is false by default");
    test.ok(settings.excludeHidden, "excludeHidden is true by default");
    test.ok(!settings.firstMatch, "firstmatch is false by default");
    test.ok(settings.linesAfter === 0, "linesAfter == 0 by default");
    test.ok(settings.linesBefore === 0, "linesBefore == 0 by default");
    test.ok(!settings.listDirs, "listDirs is false by default");
    test.ok(!settings.listFiles, "listFiles is false by default");
    test.ok(!settings.listLines, "listLines is false by default");
    test.ok(settings.maxLineLength === 150, "maxLineLength == 150 by default");
    test.ok(!settings.multilineSearch, "multilineSearch is false by default");
    test.ok(!settings.printResults, "printResults is true by default");
    test.ok(!settings.printUsage, "printUsage is false by default");
    test.ok(!settings.printVersion, "printVersion is false by default");
    test.ok(settings.recursive, "recursive is true by default");
    test.ok(!settings.searchArchives, "searchArchives is false by default");
    test.ok(settings.startPath === '', "startPath is empty by default");
    test.ok(!settings.uniqueLines, "uniqueLines is false by default");
    test.ok(!settings.verbose, "verbose is false by default");
    test.done();
};

exports.testAddExtensionsAsCommaSeparatedString = (test) => {
    let settings = new SearchSettings();
    settings.addInExtension("js,java");
    test.ok(settings.inExtensions.length === 2, "inExtensions has two extensions");
    test.ok(settings.inExtensions[0] === 'js', "first extension is js");
    test.ok(settings.inExtensions[1] === 'java', "first extension is java");
    test.done();
};

exports.testAddExtensionsAsArray = (test) => {
    let settings = new SearchSettings();
    settings.addInExtension(["js","java"]);
    test.ok(settings.inExtensions.length === 2, "inExtensions has two extensions");
    test.ok(settings.inExtensions[0] === 'js', "first extension is js");
    test.ok(settings.inExtensions[1] === 'java', "first extension is java");
    test.done();
};

exports.testAddSearchPattern = (test) => {
    let settings = new SearchSettings();
    settings.addSearchPattern("Searcher");
    test.ok(settings.searchPatterns.length === 1, "searchPatterns has one pattern");
    test.ok(settings.searchPatterns[0].source === 'Searcher', "pattern is /Searcher/");
    test.done();
};

exports.testAddSearchPatterns = (test) => {
    let settings = new SearchSettings();
    settings.addSearchPattern(["Searcher", "FileTypes"]);
    test.ok(settings.searchPatterns.length === 2, "searchPatterns has two patterns");
    test.ok(settings.searchPatterns[0].source === 'Searcher', "pattern is /Searcher/");
    test.ok(settings.searchPatterns[1].source === 'FileTypes', "pattern is /FileTypes/");
    test.done();
};

exports.testSetArchivesOnly = (test) => {
    let settings = new SearchSettings();
    test.ok(settings.archivesOnly === false, "archivesOnly is false by default");
    test.ok(settings.searchArchives === false, "searchArchives is false by default");
    settings.setArchivesOnly();
    test.ok(settings.archivesOnly === true, "archivesOnly is now true");
    test.ok(settings.searchArchives === true, "searchArchives is now true");
    test.done();
};

exports.testSetDebug = (test) => {
    let settings = new SearchSettings();
    test.ok(settings.debug === false, "debug is false by default");
    test.ok(settings.verbose === false, "verbose is false by default");
    settings.setDebug();
    test.ok(settings.debug === true, "debug is now true");
    test.ok(settings.verbose === true, "verbose is now true");
    test.done();
};
