/// <reference path="../typings/node/node.d.ts"/>
/// <reference path="../src/config.ts"/>
/// <reference path="../src/fileutil.ts"/>
/// <reference path="../src/searcher.ts"/>
/// <reference path="../src/searchresult.ts"/>
/// <reference path="../src/searchsettings.ts"/>
/*
 * searcher_test.js
 *
 * Some nodeunit tests of searcher.js
 */

var config = require('../build/config.js');
var FileUtil = require('../build/fileutil.js').FileUtil;
var Searcher = require('../build/searcher.js').Searcher;
var SearchResult = require('../build/searchresult.js').SearchResult;
var SearchSettings = require('../build/searchsettings.js').SearchSettings;

var testFile = config.SHAREDPATH + "/testFiles/testFile2.txt";

var getSettings = function() {
    var settings: SearchSettings = new SearchSettings();
    settings.startPath = '.';
    settings.addSearchPattern ('Searcher');
    return settings;
    test.done()
}

/*************************************************************
 * isSearchDir tests
 *************************************************************/
exports.testisSearchDir_SingleDot_True = function(test) {
    var settings: SearchSettings = getSettings();
    var searcher: Searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir("."));
    test.done()
}

exports.testisSearchDir_DoubleDot_True = function(test) {
    var settings: SearchSettings = getSettings();
    var searcher: Searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir(".."));
    test.done()
}

exports.testisSearchDir_IsHidden_False = function(test) {
    var settings: SearchSettings = getSettings();
    var searcher: Searcher = new Searcher(settings);
    test.ok(!searcher.isSearchDir(".git"));
    test.done()
}

exports.testisSearchDir_IsHiddenIncludeHidden_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.excludeHidden = false;
    var searcher: Searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir(".git"));
    test.done()
}

exports.testisSearchDir_NoPatterns_True = function(test) {
    var settings: SearchSettings = getSettings();
    var searcher: Searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir("/Users"));
    test.done()
}

exports.testisSearchDir_MatchesInPattern_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addInDirPattern("Search");
    var searcher: Searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir("CsSearch"));
    test.done()
}

exports.testisSearchDir_MatchesOutPattern_False = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addOutDirPattern("Search");
    var searcher: Searcher = new Searcher(settings);
    test.ok(!searcher.isSearchDir("CsSearch"));
    test.done()
}

exports.testisSearchDir_DoesNotMatchInPattern_False = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addInDirPattern("SearchFiles");
    var searcher: Searcher = new Searcher(settings);
    test.ok(!searcher.isSearchDir("CsSearch"));
    test.done()
}

exports.testisSearchDir_DoesNotMatchOutPattern_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addOutDirPattern("SearchFiles");
    var searcher: Searcher = new Searcher(settings);
    var dir: string = "CsSearch";
    test.ok(searcher.isSearchDir(dir));
    test.done()
}

/*************************************************************
 * isSearchFile tests
 *************************************************************/
exports.testIsSearchFile_NoExtensionsNoPatterns_True = function(test) {
    var settings: SearchSettings = getSettings();
    var searcher: Searcher = new Searcher(settings);
    var file: string = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_MatchesInExtension_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addInExtension("cs");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_DoesNotMatchInExtension_False = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addInExtension("java");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "FileUtil.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_MatchesOutExtension_False = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addOutExtension("cs");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "FileUtil.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_DoesNotMatchOutExtension_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addOutExtension("java");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_MatchesInPattern_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addInFilePattern("Search");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "Searcher.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_DoesNotMatchInPattern_False = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addInFilePattern("Search");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "FileUtil.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_MatchesOutPattern_False = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addOutFilePattern("Search");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "Searcher.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_DoesNotMatchOutPattern_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addOutFilePattern("Search");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
}

/*************************************************************
 * IsArchiveSearchFile tests
 *************************************************************/
exports.testIsArchiveSearchFile_NoExtensionsNoPatterns_True = function(test) {
    var settings: SearchSettings = getSettings();
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_MatchesInExtension_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addInArchiveExtension("zip");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_DoesNotMatchInExtension_False = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addInArchiveExtension("gz");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_MatchesOutExtension_False = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addOutArchiveExtension("zip");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_DoesNotMatchOutExtension_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addOutArchiveExtension("gz");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_MatchesInPattern_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addInArchiveFilePattern("arch");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_DoesNotMatchInPattern_False = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addInArchiveFilePattern("archives");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_MatchesOutPattern_False = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addOutArchiveFilePattern("arch");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_DoesNotMatchOutPattern_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addOutArchiveFilePattern("archives");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
}

/*************************************************************
 * filterFile tests
 *************************************************************/
exports.testFilterFile_IsHidden_False = function(test) {
    var settings: SearchSettings = getSettings();
    var searcher: Searcher = new Searcher(settings);
    var file: string = ".gitignore";
    test.ok(!searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_IsHiddenIncludeHidden_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.excludeHidden = false;
    var searcher: Searcher = new Searcher(settings);
    var file: string = ".gitignore";
    test.ok(searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_ArchiveNoSearchArchives_False = function(test) {
    var settings: SearchSettings = getSettings();
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(!searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_ArchiveSearchArchives_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.searchArchives = true;
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_IsArchiveSearchFile_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.searchArchives = true;
    settings.addInArchiveExtension("zip");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_NotIsArchiveSearchFile_False = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addOutExtension("zip");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(!searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_ArchiveFileArchivesOnly_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.archivesOnly = true;
    var searcher: Searcher = new Searcher(settings);
    var file: string = "archive.zip";
    test.ok(!searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_NoExtensionsNoPatterns_True = function(test) {
    var settings: SearchSettings = getSettings();
    var searcher: Searcher = new Searcher(settings);
    var file: string = "FileUtil.cs";
    test.ok(searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_IsSearchFile_True = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addInExtension("cs");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "FileUtil.cs";
    test.ok(searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_NotIsSearchFile_False = function(test) {
    var settings: SearchSettings = getSettings();
    settings.addOutExtension("cs");
    var searcher: Searcher = new Searcher(settings);
    var file: string = "FileUtil.cs";
    test.ok(!searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_NonArchiveFileArchivesOnly_False = function(test) {
    var settings: SearchSettings = getSettings();
    settings.archivesOnly = true;
    var searcher: Searcher = new Searcher(settings);
    var file: string = "FileUtil.cs";
    test.ok(!searcher.filterFile(file));
    test.done()
}

/*************************************************************
 * searchLines test
 *************************************************************/
exports.TestSearchLines = function(test) {
    var settings: SearchSettings = getSettings();
    var searcher: Searcher = new Searcher(settings);
    var lines: string[] = FileUtil.getFileLines(testFile);

    var results: SearchResult[] = searcher.searchLines(lines);
    test.ok(results.length == 2);

    var firstResult: SearchResult = results[0];
    var expectedFirstLineNum: number = 23;
    test.ok(firstResult.linenum == expectedFirstLineNum);
    var expectedFirstMatchStartIndex: number = 3;
    test.ok(firstResult.matchStartIndex === expectedFirstMatchStartIndex);
    var expectedFirstMatchEndIndex: number = 11;
    test.ok(firstResult.matchEndIndex === expectedFirstMatchEndIndex);

    var secondResult: SearchResult = results[1];
    var expectedSecondLineNum: number = 29;
    test.ok(secondResult.linenum === expectedSecondLineNum);
    var expectedSecondMatchStartIndex: number = 24;
    test.ok(secondResult.matchStartIndex === expectedSecondMatchStartIndex);
    var expectedSecondMatchEndIndex: number = 32;
    test.ok(secondResult.matchEndIndex === expectedSecondMatchEndIndex);

    test.done()
}

/*************************************************************
 * searchMultiLineString test
 *************************************************************/
exports.TestSearchMultiLineString = function(test) {
    var settings: SearchSettings = getSettings();
    var searcher: Searcher = new Searcher(settings);
    var contents: string = FileUtil.getFileContents(testFile);

    var results: SearchResult[] = searcher.searchMultiLineString(contents);
    test.ok(results.length == 2);

    var firstResult: SearchResult = results[0];
    var expectedFirstLineNum: number = 23;
    test.ok(firstResult.linenum == expectedFirstLineNum);
    var expectedFirstMatchStartIndex: number = 3;
    test.ok(firstResult.matchStartIndex === expectedFirstMatchStartIndex);
    var expectedFirstMatchEndIndex: number = 11;
    test.ok(firstResult.matchEndIndex === expectedFirstMatchEndIndex);

    var secondResult: SearchResult = results[1];
    var expectedSecondLineNum: number = 29;
    test.ok(secondResult.linenum === expectedSecondLineNum);
    var expectedSecondMatchStartIndex: number = 24;
    test.ok(secondResult.matchStartIndex === expectedSecondMatchStartIndex);
    var expectedSecondMatchEndIndex: number = 32;
    test.ok(secondResult.matchEndIndex === expectedSecondMatchEndIndex);

    test.done()
}
