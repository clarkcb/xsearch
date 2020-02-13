/*
 * searcher_test.js
 *
 * Some nodeunit tests of searcher.js
 */

import * as config from '../src/config';
import {FileUtil} from '../src/fileutil';
import {Searcher} from '../src/searcher';
import {SearchResult} from '../src/searchresult';
import {SearchSettings} from '../src/searchsettings';

const testFile = config.SHAREDPATH + '/testFiles/testFile2.txt';

const getSettings = function() {
    let settings: SearchSettings = new SearchSettings();
    settings.startPath = '.';
    settings.addSearchPattern ('Searcher');
    return settings;
};

/*************************************************************
 * isSearchDir tests
 *************************************************************/
exports.testisSearchDir_SingleDot_True = function(test) {
    const settings: SearchSettings = getSettings();
    const searcher: Searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir("."));
    test.done()
};

exports.testisSearchDir_DoubleDot_True = function(test) {
    const settings: SearchSettings = getSettings();
    const searcher: Searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir(".."));
    test.done()
};

exports.testisSearchDir_IsHidden_False = function(test) {
    const settings: SearchSettings = getSettings();
    const searcher: Searcher = new Searcher(settings);
    test.ok(!searcher.isSearchDir(".git"));
    test.done()
};

exports.testisSearchDir_IsHiddenIncludeHidden_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.excludeHidden = false;
    const searcher: Searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir(".git"));
    test.done()
};

exports.testisSearchDir_NoPatterns_True = function(test) {
    const settings: SearchSettings = getSettings();
    const searcher: Searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir("/Users"));
    test.done()
};

exports.testisSearchDir_MatchesInPattern_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addInDirPattern("Search");
    const searcher: Searcher = new Searcher(settings);
    test.ok(searcher.isSearchDir("CsSearch"));
    test.done()
};

exports.testisSearchDir_MatchesOutPattern_False = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addOutDirPattern("Search");
    const searcher: Searcher = new Searcher(settings);
    test.ok(!searcher.isSearchDir("CsSearch"));
    test.done()
};

exports.testisSearchDir_DoesNotMatchInPattern_False = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addInDirPattern("SearchFiles");
    const searcher: Searcher = new Searcher(settings);
    test.ok(!searcher.isSearchDir("CsSearch"));
    test.done()
};

exports.testisSearchDir_DoesNotMatchOutPattern_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addOutDirPattern("SearchFiles");
    const searcher: Searcher = new Searcher(settings);
    const dir: string = "CsSearch";
    test.ok(searcher.isSearchDir(dir));
    test.done()
};

/*************************************************************
 * isSearchFile tests
 *************************************************************/
exports.testIsSearchFile_NoExtensionsNoPatterns_True = function(test) {
    const settings: SearchSettings = getSettings();
    const searcher: Searcher = new Searcher(settings);
    const file: string = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_MatchesInExtension_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addInExtension("cs");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_DoesNotMatchInExtension_False = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addInExtension("java");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "FileUtil.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_MatchesOutExtension_False = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addOutExtension("cs");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "FileUtil.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_DoesNotMatchOutExtension_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addOutExtension("java");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_MatchesInPattern_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addInFilePattern("Search");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "Searcher.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_DoesNotMatchInPattern_False = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addInFilePattern("Search");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "FileUtil.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_MatchesOutPattern_False = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addOutFilePattern("Search");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "Searcher.cs";
    test.ok(!searcher.isSearchFile(file));
    test.done()
};

exports.testIsSearchFile_DoesNotMatchOutPattern_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addOutFilePattern("Search");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "FileUtil.cs";
    test.ok(searcher.isSearchFile(file));
    test.done()
};

/*************************************************************
 * IsArchiveSearchFile tests
 *************************************************************/
exports.testIsArchiveSearchFile_NoExtensionsNoPatterns_True = function(test) {
    const settings: SearchSettings = getSettings();
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_MatchesInExtension_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addInArchiveExtension("zip");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_DoesNotMatchInExtension_False = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addInArchiveExtension("gz");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_MatchesOutExtension_False = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addOutArchiveExtension("zip");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_DoesNotMatchOutExtension_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addOutArchiveExtension("gz");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_MatchesInPattern_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addInArchiveFilePattern("arch");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_DoesNotMatchInPattern_False = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addInArchiveFilePattern("archives");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_MatchesOutPattern_False = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addOutArchiveFilePattern("arch");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(!searcher.isArchiveSearchFile(file));
    test.done()
};

exports.testIsArchiveSearchFile_DoesNotMatchOutPattern_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addOutArchiveFilePattern("archives");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(searcher.isArchiveSearchFile(file));
    test.done()
};

/*************************************************************
 * filterFile tests
 *************************************************************/
exports.testFilterFile_IsHidden_False = function(test) {
    const settings: SearchSettings = getSettings();
    const searcher: Searcher = new Searcher(settings);
    const file: string = ".gitignore";
    test.ok(!searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_IsHiddenIncludeHidden_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.excludeHidden = false;
    const searcher: Searcher = new Searcher(settings);
    const file: string = ".gitignore";
    test.ok(searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_ArchiveNoSearchArchives_False = function(test) {
    const settings: SearchSettings = getSettings();
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(!searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_ArchiveSearchArchives_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.searchArchives = true;
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_IsArchiveSearchFile_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.searchArchives = true;
    settings.addInArchiveExtension("zip");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_NotIsArchiveSearchFile_False = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addOutExtension("zip");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(!searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_ArchiveFileArchivesOnly_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.archivesOnly = true;
    const searcher: Searcher = new Searcher(settings);
    const file: string = "archive.zip";
    test.ok(!searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_NoExtensionsNoPatterns_True = function(test) {
    const settings: SearchSettings = getSettings();
    const searcher: Searcher = new Searcher(settings);
    const file: string = "FileUtil.cs";
    test.ok(searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_IsSearchFile_True = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addInExtension("cs");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "FileUtil.cs";
    test.ok(searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_NotIsSearchFile_False = function(test) {
    let settings: SearchSettings = getSettings();
    settings.addOutExtension("cs");
    const searcher: Searcher = new Searcher(settings);
    const file: string = "FileUtil.cs";
    test.ok(!searcher.filterFile(file));
    test.done()
};

exports.testFilterFile_NonArchiveFileArchivesOnly_False = function(test) {
    let settings: SearchSettings = getSettings();
    settings.archivesOnly = true;
    const searcher: Searcher = new Searcher(settings);
    const file: string = "FileUtil.cs";
    test.ok(!searcher.filterFile(file));
    test.done()
};

/*************************************************************
 * searchLines test
 *************************************************************/
exports.TestSearchLines = function(test) {
    const settings: SearchSettings = getSettings();
    const searcher: Searcher = new Searcher(settings);
    const lines: string[] = FileUtil.getFileLines(testFile, 'utf-8');

    const results: SearchResult[] = searcher.searchLines(lines);
    test.ok(results.length == 2);

    const firstResult: SearchResult = results[0];
    const expectedFirstLineNum: number = 29;
    test.ok(firstResult.linenum == expectedFirstLineNum);
    const expectedFirstMatchStartIndex: number = 3;
    test.ok(firstResult.matchStartIndex === expectedFirstMatchStartIndex);
    const expectedFirstMatchEndIndex: number = 11;
    test.ok(firstResult.matchEndIndex === expectedFirstMatchEndIndex);

    const secondResult: SearchResult = results[1];
    const expectedSecondLineNum: number = 35;
    test.ok(secondResult.linenum === expectedSecondLineNum);
    const expectedSecondMatchStartIndex: number = 24;
    test.ok(secondResult.matchStartIndex === expectedSecondMatchStartIndex);
    const expectedSecondMatchEndIndex: number = 32;
    test.ok(secondResult.matchEndIndex === expectedSecondMatchEndIndex);

    test.done()
};

/*************************************************************
 * searchMultiLineString test
 *************************************************************/
exports.TestSearchMultiLineString = function(test) {
    const settings: SearchSettings = getSettings();
    const searcher: Searcher = new Searcher(settings);
    const contents: string = FileUtil.getFileContents(testFile, 'utf-8');

    const results: SearchResult[] = searcher.searchMultiLineString(contents);
    test.ok(results.length == 2);

    const firstResult: SearchResult = results[0];
    const expectedFirstLineNum: number = 29;
    test.ok(firstResult.linenum == expectedFirstLineNum);
    const expectedFirstMatchStartIndex: number = 3;
    test.ok(firstResult.matchStartIndex === expectedFirstMatchStartIndex);
    const expectedFirstMatchEndIndex: number = 11;
    test.ok(firstResult.matchEndIndex === expectedFirstMatchEndIndex);

    const secondResult: SearchResult = results[1];
    const expectedSecondLineNum: number = 35;
    test.ok(secondResult.linenum === expectedSecondLineNum);
    const expectedSecondMatchStartIndex: number = 24;
    test.ok(secondResult.matchStartIndex === expectedSecondMatchStartIndex);
    const expectedSecondMatchEndIndex: number = 32;
    test.ok(secondResult.matchEndIndex === expectedSecondMatchEndIndex);

    test.done()
};
