/// <reference path="../typings/node/node.d.ts"/>
/// <reference path="../src/searchoptions.ts"/>
/// <reference path="../src/searchsettings.ts"/>
/*
 * searchoptions_test.js
 *
 * Some nodeunit tests of searchoptions.js
 */

var SearchOptions = require('../build/searchoptions.js').SearchOptions;
var SearchSettings = require('../build/searchsettings.js').SearchSettings;

exports.testNoArgs = function(test) {
    var searchOptions: SearchOptions = new SearchOptions();
    searchOptions.settingsFromArgs([], function(err: Error, settings: SearchSettings) {
        if (err) {
            test.ok(false, "There was an error calling settingsFromArgs: "+ err);
            test.done();
        }
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
        test.ok(settings.printResults, "printResults is set to true in settingsFromArgs");
        test.ok(!settings.printUsage, "printUsage is false by default");
        test.ok(!settings.printVersion, "printVersion is false by default");
        test.ok(settings.recursive, "recursive is true by default");
        test.ok(!settings.searchArchives, "searchArchives is false by default");
        test.ok(settings.startPath === '', "startPath is empty by default");
        test.ok(!settings.uniqueLines, "uniqueLines is false by default");
        test.ok(!settings.verbose, "verbose is false by default");
        test.done();
    });
}

exports.testValidArgs = function(test) {
    var searchOptions: SearchOptions = new SearchOptions();
    var args: string[] = ['-x', 'js,java', '-s', 'Searcher', '.'];
    searchOptions.settingsFromArgs(args, function(err: Error, settings: SearchSettings) {
        if (err) {
            test.ok(false, "There was an error calling settingsFromArgs: "+ err);
            test.done();
            return;
        }
        test.ok(settings.inExtensions.length === 2, "inExtensions has two extensions");
        test.ok(settings.inExtensions[0] === 'js', "first inExtension is js");
        test.ok(settings.inExtensions[1] === 'java', "first inExtension is java");
        test.ok(settings.searchPatterns.length === 1, "searchPatterns has one pattern");
        test.ok(settings.searchPatterns[0].source === 'Searcher', "pattern === Searcher");
        test.done();
    });
}

exports.testArchivesOnly = function(test) {
    var searchOptions: SearchOptions = new SearchOptions();
    var args: string[] = ['--archivesonly'];
    searchOptions.settingsFromArgs(args, function(err: Error, settings: SearchSettings) {
        if (err) {
            test.ok(false, "There was an error calling settingsFromArgs: "+ err);
            test.done();
        }
        test.ok(settings.archivesOnly === true, "archivesOnly is true");
        test.ok(settings.searchArchives === true, "searchArchives is true");
        test.done();
    });
}

exports.testDebug = function(test) {
    var searchOptions: SearchOptions = new SearchOptions();
    var args: string[] = ['--debug'];
    searchOptions.settingsFromArgs(args, function(err: Error, settings: SearchSettings) {
        if (err) {
            test.ok(false, "There was an error calling settingsFromArgs: "+ err);
            test.done();
        }
        test.ok(settings.debug === true, "debug is true");
        test.ok(settings.verbose === true, "verbose is true");
        test.done();
    });
}

exports.testMissingArg = function(test) {
    var searchOptions: SearchOptions = new SearchOptions();
    var args: string[] = ['-x'];
    searchOptions.settingsFromArgs(args, function(err: Error, settings: SearchSettings) {
        if (err) {
            var expected = "Missing argument for option x";
            test.ok(err.message === expected, "Got missing argument err");
            test.done();
        } else {
            test.ok(false, "Did not get expected missing argument error");
            test.done();
        }
    });
}

exports.testInvalidArg = function(test) {
    var searchOptions: SearchOptions = new SearchOptions();
    var args: string[] = ['-Q'];
    searchOptions.settingsFromArgs(args, function(err: Error, settings: SearchSettings) {
        if (err) {
            var expected = "Invalid option: Q";
            test.ok(err.message === expected, "Got invalid option err");
            test.done();
        } else {
            test.ok(false, "Did not get expected invalid option error");
            test.done();
        }
    });
}
