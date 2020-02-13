/*
 * searchsettings_test.js
 *
 * Some nodeunit tests of searchsettings.js
 */

import {SearchSettings} from '../src/searchsettings';

exports.testDefaultSettings = function(test) {
    const settings: SearchSettings = new SearchSettings();
    test.ok(!settings.archivesOnly, "archivesOnly is false by default");
    test.ok(!settings.debug, "debug is false by default");
    test.ok(settings.excludeHidden, "excludeHidden is true by default");
    test.ok(!settings.firstMatch, "firstmatch is false by default");
    test.ok(settings.linesAfter === 0, "linesAfter == 0 by default");
    test.ok(settings.linesBefore === 0, "linesBefore == 0 by default");
    test.ok(!settings.listDirs, "listDirs is false by default");
    test.ok(!settings.listFiles, "listFiles is false by default");
    test.ok(!settings.listLines, "listLines is false by default");
    test.ok(settings.maxLineLength == 150, "maxLineLength == 150 by default");
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

exports.testAddExtensions = function(test) {
    let settings: SearchSettings = new SearchSettings();
    settings.addInExtension("js,java");
    test.ok(settings.inExtensions.length === 2, "inExtensions has two extensions");
    test.ok(settings.inExtensions[0] === 'js', "first extension is js");
    test.ok(settings.inExtensions[1] === 'java', "first extension is java");
    test.done();
};

exports.testAddSearchPattern = function(test) {
    let settings: SearchSettings = new SearchSettings();
    settings.addSearchPattern("Searcher");
    test.ok(settings.searchPatterns.length === 1, "searchPatterns has one pattern");
    test.ok(settings.searchPatterns[0].source === 'Searcher', "pattern is /Searcher/");
    test.done();
};

exports.testSetArchivesOnly = function(test) {
    let settings: SearchSettings = new SearchSettings();
    test.ok(!settings.archivesOnly, "archivesOnly is false by default");
    test.ok(!settings.searchArchives, "searchArchives is false by default");
    settings.setArchivesOnly(true);
    test.ok(settings.archivesOnly, "archivesOnly is now true");
    test.ok(settings.searchArchives, "searchArchives is now true");
    test.done();
};

exports.testSetDebug = function(test) {
    let settings: SearchSettings = new SearchSettings();
    test.ok(!settings.debug, "debug is false by default");
    test.ok(!settings.verbose, "verbose is false by default");
    settings.setDebug(true);
    test.ok(settings.debug, "debug is now true");
    test.ok(settings.verbose, "verbose is now true");
    test.done();
};
