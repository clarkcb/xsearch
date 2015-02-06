/*
 * searcher_test.js
 *
 * Some nodeunit tests of searcher.js
 */

//var FileType = require('../nodesearch/filetype.js').FileType;
var FileUtil = require('../nodesearch/fileutil.js').FileUtil;
var Searcher = require('../nodesearch/searcher.js').Searcher;
var SearchSettings = require('../nodesearch/searchsettings.js').SearchSettings;

var testFile = FileUtil.expandPath("~/src/git/xsearch/shared/testFiles/testFile2.txt");

var getSettings = function() {
    var settings = new SearchSettings();
    settings.startPath = '.';
    settings.addSearchPattern ('Searcher');
    return settings;
    test.done()
}

/*************************************************************
 * isSearchDir tests
 *************************************************************/
exports.testisSearchDir_SingleDot_True = function(test) {
    var settings = getSettings();
    var searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir("."));
    test.done()
}

exports.testisSearchDir_DoubleDot_True = function(test) {
    var settings = getSettings();
    var searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir(".."));
    test.done()
}

exports.testisSearchDir_IsHidden_False = function(test) {
    var settings = getSettings();
    var searcher = new Searcher(settings);
    test.ok(!searcher.isSearchDir(".git"));
    test.done()
}

exports.testisSearchDir_IsHiddenIncludeHidden_True = function(test) {
    var settings = getSettings();
    settings.excludeHidden = false;
    var searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir(".git"));
    test.done()
}

exports.testisSearchDir_NoPatterns_True = function(test) {
    var settings = getSettings();
    var searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir("/Users"));
    test.done()
}

exports.testisSearchDir_MatchesInPattern_True = function(test) {
    var settings = getSettings();
    settings.addInDirPattern("Search");
    var searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir("CsSearch"));
    test.done()
}

exports.testisSearchDir_MatchesOutPattern_False = function(test) {
    var settings = getSettings();
    settings.addOutDirPattern("Search");
    var searcher = new Searcher(settings);
    test.ok(!searcher.isSearchDir("CsSearch"));
    test.done()
}

exports.testisSearchDir_DoesNotMatchInPattern_False = function(test) {
    var settings = getSettings();
    settings.addInDirPattern("SearchFiles");
    var searcher = new Searcher(settings);
    test.ok(!searcher.isSearchDir("CsSearch"));
    test.done()
}

exports.testisSearchDir_DoesNotMatchOutPattern_True = function(test) {
    var settings = getSettings();
    settings.addOutDirPattern("SearchFiles");
    var searcher = new Searcher(settings);
    var dir = "CsSearch";
    test.ok(searcher.isSearchDir(dir));
    test.done()
}

/*************************************************************
 * isSearchFile tests
 *************************************************************/
exports.testIsSearchFile_NoExtensionsNoPatterns_True = function(test) {
    var settings = getSettings();
    var searcher = new Searcher(settings);
    var file = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_MatchesInExtension_True = function(test) {
    var settings = getSettings();
    settings.addInExtension("cs");
    var searcher = new Searcher(settings);
    var file = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_DoesNotMatchInExtension_False = function(test) {
    var settings = getSettings();
    settings.addInExtension("java");
    var searcher = new Searcher(settings);
    var file = "FileUtil.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_MatchesOutExtension_False = function(test) {
    var settings = getSettings();
    settings.addOutExtension("cs");
    var searcher = new Searcher(settings);
    var file = "FileUtil.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_DoesNotMatchOutExtension_True = function(test) {
    var settings = getSettings();
    settings.addOutExtension("java");
    var searcher = new Searcher(settings);
    var file = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_MatchesInPattern_True = function(test) {
    var settings = getSettings();
    settings.addInFilePattern("Search");
    var searcher = new Searcher(settings);
    var file = "Searcher.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_DoesNotMatchInPattern_False = function(test) {
    var settings = getSettings();
    settings.addInFilePattern("Search");
    var searcher = new Searcher(settings);
    var file = "FileUtil.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_MatchesOutPattern_False = function(test) {
    var settings = getSettings();
    settings.addOutFilePattern("Search");
    var searcher = new Searcher(settings);
    var file = "Searcher.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
}

exports.testIsSearchFile_DoesNotMatchOutPattern_True = function(test) {
    var settings = getSettings();
    settings.addOutFilePattern("Search");
    var searcher = new Searcher(settings);
    var file = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
}

/*************************************************************
 * IsArchiveSearchFile tests
 *************************************************************/
exports.testIsArchiveSearchFile_NoExtensionsNoPatterns_True = function(test) {
    var settings = getSettings();
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_MatchesInExtension_True = function(test) {
    var settings = getSettings();
    settings.addInArchiveExtension("zip");
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_DoesNotMatchInExtension_False = function(test) {
    var settings = getSettings();
    settings.addInArchiveExtension("gz");
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_MatchesOutExtension_False = function(test) {
    var settings = getSettings();
    settings.addOutArchiveExtension("zip");
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_DoesNotMatchOutExtension_True = function(test) {
    var settings = getSettings();
    settings.addOutArchiveExtension("gz");
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_MatchesInPattern_True = function(test) {
    var settings = getSettings();
    settings.addInArchiveFilePattern("arch");
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_DoesNotMatchInPattern_False = function(test) {
    var settings = getSettings();
    settings.addInArchiveFilePattern("archives");
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_MatchesOutPattern_False = function(test) {
    var settings = getSettings();
    settings.addOutArchiveFilePattern("arch");
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
}

exports.testIsArchiveSearchFile_DoesNotMatchOutPattern_True = function(test) {
    var settings = getSettings();
    settings.addOutArchiveFilePattern("archives");
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
}

/*************************************************************
 * filterFile tests
 *************************************************************/
exports.testFilterFile_IsHidden_False = function(test) {
    var settings = getSettings();
    var searcher = new Searcher(settings);
    var file = ".gitignore";
    test.ok(!searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_IsHiddenIncludeHidden_True = function(test) {
    var settings = getSettings();
    settings.excludeHidden = false;
    var searcher = new Searcher(settings);
    var file = ".gitignore";
    test.ok(searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_ArchiveNoSearchArchives_False = function(test) {
    var settings = getSettings();
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(!searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_ArchiveSearchArchives_True = function(test) {
    var settings = getSettings();
    settings.searchArchives = true;
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_IsArchiveSearchFile_True = function(test) {
    var settings = getSettings();
    settings.searchArchives = true;
    settings.addInArchiveExtension("zip");
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_NotIsArchiveSearchFile_False = function(test) {
    var settings = getSettings();
    settings.addOutExtension("zip");
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(!searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_ArchiveFileArchivesOnly_True = function(test) {
    var settings = getSettings();
    settings.archivesOnly = true;
    var searcher = new Searcher(settings);
    var file = "archive.zip";
    test.ok(!searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_NoExtensionsNoPatterns_True = function(test) {
    var settings = getSettings();
    var searcher = new Searcher(settings);
    var file = "FileUtil.cs";
    test.ok(searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_IsSearchFile_True = function(test) {
    var settings = getSettings();
    settings.addInExtension("cs");
    var searcher = new Searcher(settings);
    var file = "FileUtil.cs";
    test.ok(searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_NotIsSearchFile_False = function(test) {
    var settings = getSettings();
    settings.addOutExtension("cs");
    var searcher = new Searcher(settings);
    var file = "FileUtil.cs";
    test.ok(!searcher.filterFile(file));
    test.done()
}

exports.testFilterFile_NonArchiveFileArchivesOnly_False = function(test) {
    var settings = getSettings();
    settings.archivesOnly = true;
    var searcher = new Searcher(settings);
    var file = "FileUtil.cs";
    test.ok(!searcher.filterFile(file));
    test.done()
}

/*************************************************************
 * searchLines test
 *************************************************************/
exports.TestSearchLines = function(test) {
    var settings = getSettings();
    var searcher = new Searcher(settings);
    var lines = FileUtil.getFileLines(testFile);

    var results = searcher.searchLines(lines);
    test.ok(results.length == 2);

    var firstResult = results[0];
    var expectedFirstLineNum = 23;
    test.ok(firstResult.linenum == expectedFirstLineNum);
    var expectedFirstMatchStartIndex = 3;
    test.ok(firstResult.matchStartIndex === expectedFirstMatchStartIndex);
    var expectedFirstMatchEndIndex = 11;
    test.ok(firstResult.matchEndIndex === expectedFirstMatchEndIndex);

    var secondResult = results[1];
    var expectedSecondLineNum = 29;
    test.ok(secondResult.linenum === expectedSecondLineNum);
    var expectedSecondMatchStartIndex = 24;
    test.ok(secondResult.matchStartIndex === expectedSecondMatchStartIndex);
    var expectedSecondMatchEndIndex = 32;
    test.ok(secondResult.matchEndIndex === expectedSecondMatchEndIndex);

    test.done()
}

/*************************************************************
 * searchMultiLineString test
 *************************************************************/
exports.TestSearchMultiLineString = function(test) {
    var settings = getSettings();
    var searcher = new Searcher(settings);
    var contents = FileUtil.getFileContents(testFile);

    var results = searcher.searchMultiLineString(contents);
    test.ok(results.length == 2);

    var firstResult = results[0];
    var expectedFirstLineNum = 23;
    test.ok(firstResult.linenum == expectedFirstLineNum);
    var expectedFirstMatchStartIndex = 3;
    test.ok(firstResult.matchStartIndex === expectedFirstMatchStartIndex);
    var expectedFirstMatchEndIndex = 11;
    test.ok(firstResult.matchEndIndex === expectedFirstMatchEndIndex);

    var secondResult = results[1];
    var expectedSecondLineNum = 29;
    test.ok(secondResult.linenum === expectedSecondLineNum);
    var expectedSecondMatchStartIndex = 24;
    test.ok(secondResult.matchStartIndex === expectedSecondMatchStartIndex);
    var expectedSecondMatchEndIndex = 32;
    test.ok(secondResult.matchEndIndex === expectedSecondMatchEndIndex);

    test.done()
}
