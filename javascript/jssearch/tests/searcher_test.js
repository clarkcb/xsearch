/*
 * searcher_test.js
 *
 * Some nodeunit tests of searcher.js
 */

const config = require('../src/config.js');
const FileUtil = require('../src/fileutil.js').FileUtil;
const Searcher = require('../src/searcher.js').Searcher;
const SearchSettings = require('../src/searchsettings.js').SearchSettings;

const testFile = config.SHAREDPATH + "/testFiles/testFile2.txt";

const getSettings = function() {
    var settings = new SearchSettings();
    settings.startPath = '.';
    settings.addSearchPattern ('Searcher');
    return settings;
};

/*************************************************************
 * isSearchDir tests
 *************************************************************/
exports.testisSearchDir_SingleDot_True = function(test) {
    const settings = getSettings();
    const searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir("."));
    test.done()
};

exports.testisSearchDir_DoubleDot_True = function(test) {
    const settings = getSettings();
    const searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir(".."));
    test.done()
};

exports.testisSearchDir_IsHidden_False = function(test) {
    const settings = getSettings();
    const searcher = new Searcher(settings);
    test.ok(!searcher.isSearchDir(".git"));
    test.done()
};

exports.testisSearchDir_IsHiddenIncludeHidden_True = function(test) {
    const settings = getSettings();
    settings.excludeHidden = false;
    const searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir(".git"));
    test.done()
};

exports.testisSearchDir_NoPatterns_True = function(test) {
    const settings = getSettings();
    const searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir("/Users"));
    test.done()
};

exports.testisSearchDir_MatchesInPattern_True = function(test) {
    const settings = getSettings();
    settings.addInDirPattern("Search");
    const searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir("CsSearch"));
    test.done()
};

exports.testisSearchDir_MatchesOutPattern_False = function(test) {
    const settings = getSettings();
    settings.addOutDirPattern("Search");
    const searcher = new Searcher(settings);
    test.ok(!searcher.isSearchDir("CsSearch"));
    test.done()
};

exports.testisSearchDir_DoesNotMatchInPattern_False = function(test) {
    const settings = getSettings();
    settings.addInDirPattern("SearchFiles");
    const searcher = new Searcher(settings);
    test.ok(!searcher.isSearchDir("CsSearch"));
    test.done()
};

exports.testisSearchDir_DoesNotMatchOutPattern_True = function(test) {
    const settings = getSettings();
    settings.addOutDirPattern("SearchFiles");
    const searcher = new Searcher(settings);
    const dir = "CsSearch";
    test.ok(searcher.isSearchDir(dir));
    test.done()
};

/*************************************************************
 * isSearchFile tests
 *************************************************************/
exports.testIsSearchFile_NoExtensionsNoPatterns_True = function(test) {
    const settings = getSettings();
    const searcher = new Searcher(settings);
    const file = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_MatchesInExtension_True = function(test) {
    const settings = getSettings();
    settings.addInExtension("cs");
    const searcher = new Searcher(settings);
    const file = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_DoesNotMatchInExtension_False = function(test) {
    const settings = getSettings();
    settings.addInExtension("java");
    const searcher = new Searcher(settings);
    const file = "FileUtil.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_MatchesOutExtension_False = function(test) {
    const settings = getSettings();
    settings.addOutExtension("cs");
    const searcher = new Searcher(settings);
    const file = "FileUtil.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_DoesNotMatchOutExtension_True = function(test) {
    const settings = getSettings();
    settings.addOutExtension("java");
    const searcher = new Searcher(settings);
    const file = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_MatchesInPattern_True = function(test) {
    const settings = getSettings();
    settings.addInFilePattern("Search");
    const searcher = new Searcher(settings);
    const file = "Searcher.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_DoesNotMatchInPattern_False = function(test) {
    const settings = getSettings();
    settings.addInFilePattern("Search");
    const searcher = new Searcher(settings);
    const file = "FileUtil.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_MatchesOutPattern_False = function(test) {
    const settings = getSettings();
    settings.addOutFilePattern("Search");
    const searcher = new Searcher(settings);
    const file = "Searcher.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_DoesNotMatchOutPattern_True = function(test) {
    const settings = getSettings();
    settings.addOutFilePattern("Search");
    const searcher = new Searcher(settings);
    const file = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
};

/*************************************************************
 * IsArchiveSearchFile tests
 *************************************************************/
exports.testIsArchiveSearchFile_NoExtensionsNoPatterns_True = function(test) {
    const settings = getSettings();
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_MatchesInExtension_True = function(test) {
    const settings = getSettings();
    settings.addInArchiveExtension("zip");
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_DoesNotMatchInExtension_False = function(test) {
    const settings = getSettings();
    settings.addInArchiveExtension("gz");
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_MatchesOutExtension_False = function(test) {
    const settings = getSettings();
    settings.addOutArchiveExtension("zip");
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_DoesNotMatchOutExtension_True = function(test) {
    const settings = getSettings();
    settings.addOutArchiveExtension("gz");
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_MatchesInPattern_True = function(test) {
    const settings = getSettings();
    settings.addInArchiveFilePattern("arch");
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_DoesNotMatchInPattern_False = function(test) {
    const settings = getSettings();
    settings.addInArchiveFilePattern("archives");
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_MatchesOutPattern_False = function(test) {
    const settings = getSettings();
    settings.addOutArchiveFilePattern("arch");
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_DoesNotMatchOutPattern_True = function(test) {
    const settings = getSettings();
    settings.addOutArchiveFilePattern("archives");
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
};

/*************************************************************
 * filterFile tests
 *************************************************************/
exports.testFilterFile_IsHidden_False = function(test) {
    const settings = getSettings();
    const searcher = new Searcher(settings);
    const file = ".gitignore";
    test.ok(!searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_IsHiddenIncludeHidden_True = function(test) {
    const settings = getSettings();
    settings.excludeHidden = false;
    const searcher = new Searcher(settings);
    const file = ".gitignore";
    test.ok(searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_ArchiveNoSearchArchives_False = function(test) {
    const settings = getSettings();
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(!searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_ArchiveSearchArchives_True = function(test) {
    const settings = getSettings();
    settings.searchArchives = true;
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_IsArchiveSearchFile_True = function(test) {
    const settings = getSettings();
    settings.searchArchives = true;
    settings.addInArchiveExtension("zip");
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_NotIsArchiveSearchFile_False = function(test) {
    const settings = getSettings();
    settings.addOutExtension("zip");
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(!searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_ArchiveFileArchivesOnly_True = function(test) {
    const settings = getSettings();
    settings.archivesOnly = true;
    const searcher = new Searcher(settings);
    const file = "archive.zip";
    test.ok(!searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_NoExtensionsNoPatterns_True = function(test) {
    const settings = getSettings();
    const searcher = new Searcher(settings);
    const file = "FileUtil.cs";
    test.ok(searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_IsSearchFile_True = function(test) {
    const settings = getSettings();
    settings.addInExtension("cs");
    const searcher = new Searcher(settings);
    const file = "FileUtil.cs";
    test.ok(searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_NotIsSearchFile_False = function(test) {
    const settings = getSettings();
    settings.addOutExtension("cs");
    const searcher = new Searcher(settings);
    const file = "FileUtil.cs";
    test.ok(!searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_NonArchiveFileArchivesOnly_False = function(test) {
    const settings = getSettings();
    settings.archivesOnly = true;
    const searcher = new Searcher(settings);
    const file = "FileUtil.cs";
    test.ok(!searcher.filterFile(file));
    test.done()
};

/*************************************************************
 * searchLines test
 *************************************************************/
exports.TestSearchLines = function(test) {
    const settings = getSettings();
    const searcher = new Searcher(settings);
    const lines = FileUtil.getFileLines(testFile);

    const results = searcher.searchLines(lines);
    test.ok(results.length === 2);

    const firstResult = results[0];
    const expectedFirstLineNum = 29;
    test.ok(firstResult.linenum === expectedFirstLineNum);
    const expectedFirstMatchStartIndex = 3;
    test.ok(firstResult.matchStartIndex === expectedFirstMatchStartIndex);
    const expectedFirstMatchEndIndex = 11;
    test.ok(firstResult.matchEndIndex === expectedFirstMatchEndIndex);

    const secondResult = results[1];
    const expectedSecondLineNum = 35;
    test.ok(secondResult.linenum === expectedSecondLineNum);
    const expectedSecondMatchStartIndex = 24;
    test.ok(secondResult.matchStartIndex === expectedSecondMatchStartIndex);
    const expectedSecondMatchEndIndex = 32;
    test.ok(secondResult.matchEndIndex === expectedSecondMatchEndIndex);

    test.done()
};

/*************************************************************
 * searchMultiLineString test
 *************************************************************/
exports.TestSearchMultiLineString = function(test) {
    const settings = getSettings();
    const searcher = new Searcher(settings);
    const contents = FileUtil.getFileContents(testFile);

    const results = searcher.searchMultiLineString(contents);
    test.ok(results.length === 2);

    const firstResult = results[0];
    const expectedFirstLineNum = 29;
    test.ok(firstResult.linenum === expectedFirstLineNum);
    const expectedFirstMatchStartIndex = 3;
    test.ok(firstResult.matchStartIndex === expectedFirstMatchStartIndex);
    const expectedFirstMatchEndIndex = 11;
    test.ok(firstResult.matchEndIndex === expectedFirstMatchEndIndex);

    const secondResult = results[1];
    const expectedSecondLineNum = 35;
    test.ok(secondResult.linenum === expectedSecondLineNum);
    const expectedSecondMatchStartIndex = 24;
    test.ok(secondResult.matchStartIndex === expectedSecondMatchStartIndex);
    const expectedSecondMatchEndIndex = 32;
    test.ok(secondResult.matchEndIndex === expectedSecondMatchEndIndex);

    test.done()
};
