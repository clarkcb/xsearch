/*
 * searcher.test.js
 *
 * Some tests of searcher.js
 */

import * as config from '../src/config';
import {FileUtil} from '../src/fileutil';
import {Searcher} from '../src/searcher';
import {SearchResult} from '../src/searchresult';
import {SearchSettings} from '../src/searchsettings';

const testFile = config.SHAREDPATH + '/testFiles/testFile2.txt';

const getSettings = function() {
    const settings: SearchSettings = new SearchSettings();
    settings.startPath = '.';
    settings.addSearchPatterns('Searcher');
    return settings;
};

describe('testing searcher', () => {

    /*************************************************************
     * isSearchDir tests
     *************************************************************/
    it('testisSearchDir_SingleDot_True', () => {
        const settings: SearchSettings = getSettings();
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchDir(".")).toBeTruthy();
    });

    it('testisSearchDir_DoubleDot_True', () => {
        const settings: SearchSettings = getSettings();
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchDir("..")).toBeTruthy();
    });

    it('testisSearchDir_IsHidden_False', () => {
        const settings: SearchSettings = getSettings();
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchDir(".git")).toBeFalsy();
    });

    it('testisSearchDir_IsHiddenIncludeHidden_True', () => {
        const settings: SearchSettings = getSettings();
        settings.excludeHidden = false;
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchDir(".git")).toBeTruthy();
    });

    it('testisSearchDir_NoPatterns_True', () => {
        const settings: SearchSettings = getSettings();
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchDir("/Users")).toBeTruthy();
    });

    it('testisSearchDir_MatchesInPattern_True', () => {
        const settings: SearchSettings = getSettings();
        settings.addInDirPatterns("Search");
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchDir("CsSearch")).toBeTruthy();
    });

    it('testisSearchDir_MatchesOutPattern_False', () => {
        const settings: SearchSettings = getSettings();
        settings.addOutDirPatterns("Search");
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchDir("CsSearch")).toBeFalsy();
    });

    it('testisSearchDir_DoesNotMatchInPattern_False', () => {
        const settings: SearchSettings = getSettings();
        settings.addInDirPatterns("SearchFiles");
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchDir("CsSearch")).toBeFalsy();
    });

    it('testisSearchDir_DoesNotMatchOutPattern_True', () => {
        const settings: SearchSettings = getSettings();
        settings.addOutDirPatterns("SearchFiles");
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchDir("CsSearch")).toBeTruthy();
    });

    /*************************************************************
     * isSearchFile tests
     *************************************************************/
    it('testIsSearchFile_NoExtensionsNoPatterns_True', () => {
        const settings: SearchSettings = getSettings();
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchFile("FileUtil.cs")).toBeTruthy();
    });

    it('testIsSearchFile_MatchesInExtension_True', () => {
        const settings: SearchSettings = getSettings();
        settings.addInExtensions("cs");
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchFile("FileUtil.cs")).toBeTruthy();
    });

    it('testIsSearchFile_DoesNotMatchInExtension_False', () => {
        const settings: SearchSettings = getSettings();
        settings.addInExtensions("java");
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchFile("FileUtil.cs")).toBeFalsy();
    });

    it('testIsSearchFile_MatchesOutExtension_False', () => {
        const settings: SearchSettings = getSettings();
        settings.addOutExtensions("cs");
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchFile("FileUtil.cs")).toBeFalsy();
    });

    it('testIsSearchFile_DoesNotMatchOutExtension_True', () => {
        const settings: SearchSettings = getSettings();
        settings.addOutExtensions("java");
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchFile("FileUtil.cs")).toBeTruthy();
    });

    it('testIsSearchFile_MatchesInPattern_True', () => {
        const settings: SearchSettings = getSettings();
        settings.addInFilePatterns("Search");
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchFile("Searcher.cs")).toBeTruthy();
    });

    it('testIsSearchFile_DoesNotMatchInPattern_False', () => {
        const settings: SearchSettings = getSettings();
        settings.addInFilePatterns("Search");
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchFile("FileUtil.cs")).toBeFalsy();
    });

    it('testIsSearchFile_MatchesOutPattern_False', () => {
        const settings: SearchSettings = getSettings();
        settings.addOutFilePatterns("Search");
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchFile("Searcher.cs")).toBeFalsy();
    });

    it('testIsSearchFile_DoesNotMatchOutPattern_True', () => {
        const settings: SearchSettings = getSettings();
        settings.addOutFilePatterns("Search");
        const searcher: Searcher = new Searcher(settings);
        expect(searcher.isSearchFile("FileUtil.cs")).toBeTruthy();
    });

    /*************************************************************
     * IsArchiveSearchFile tests
     *************************************************************/
    it('testIsArchiveSearchFile_NoExtensionsNoPatterns_True', () => {
        const settings: SearchSettings = getSettings();
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeTruthy();
    });

    it('testIsArchiveSearchFile_MatchesInExtension_True', () => {
        const settings: SearchSettings = getSettings();
        settings.addInArchiveExtensions("zip");
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeTruthy();
    });

    it('testIsArchiveSearchFile_DoesNotMatchInExtension_False', () => {
        const settings: SearchSettings = getSettings();
        settings.addInArchiveExtensions("gz");
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeFalsy();
    });

    it('testIsArchiveSearchFile_MatchesOutExtension_False', () => {
        const settings: SearchSettings = getSettings();
        settings.addOutArchiveExtensions("zip");
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeFalsy();
    });

    it('testIsArchiveSearchFile_DoesNotMatchOutExtension_True', () => {
        const settings: SearchSettings = getSettings();
        settings.addOutArchiveExtensions("gz");
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeTruthy();
    });

    it('testIsArchiveSearchFile_MatchesInPattern_True', () => {
        const settings: SearchSettings = getSettings();
        settings.addInArchiveFilePatterns("arch");
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeTruthy();
    });

    it('testIsArchiveSearchFile_DoesNotMatchInPattern_False', () => {
        const settings: SearchSettings = getSettings();
        settings.addInArchiveFilePatterns("archives");
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeFalsy();
    });

    it('testIsArchiveSearchFile_MatchesOutPattern_False', () => {
        const settings: SearchSettings = getSettings();
        settings.addOutArchiveFilePatterns("arch");
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeFalsy();
    });

    it('testIsArchiveSearchFile_DoesNotMatchOutPattern_True', () => {
        const settings: SearchSettings = getSettings();
        settings.addOutArchiveFilePatterns("archives");
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeTruthy();
    });

    /*************************************************************
     * filterFile tests
     *************************************************************/
    it('testFilterFile_IsHidden_False', () => {
        const settings: SearchSettings = getSettings();
        const searcher: Searcher = new Searcher(settings);
        const file = ".gitignore";
        expect(searcher.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_IsHiddenIncludeHidden_True', () => {
        const settings: SearchSettings = getSettings();
        settings.excludeHidden = false;
        const searcher: Searcher = new Searcher(settings);
        const file = ".gitignore";
        expect(searcher.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_ArchiveNoSearchArchives_False', () => {
        const settings: SearchSettings = getSettings();
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_ArchiveSearchArchives_True', () => {
        const settings: SearchSettings = getSettings();
        settings.searchArchives = true;
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_IsArchiveSearchFile_True', () => {
        const settings: SearchSettings = getSettings();
        settings.searchArchives = true;
        settings.addInArchiveExtensions("zip");
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_NotIsArchiveSearchFile_False', () => {
        const settings: SearchSettings = getSettings();
        settings.addOutExtensions("zip");
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_ArchiveFileArchivesOnly_True', () => {
        const settings: SearchSettings = getSettings();
        settings.archivesOnly = true;
        const searcher: Searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_NoExtensionsNoPatterns_True', () => {
        const settings: SearchSettings = getSettings();
        const searcher: Searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_IsSearchFile_True', () => {
        const settings: SearchSettings = getSettings();
        settings.addInExtensions("cs");
        const searcher: Searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_NotIsSearchFile_False', () => {
        const settings: SearchSettings = getSettings();
        settings.addOutExtensions("cs");
        const searcher: Searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_NonArchiveFileArchivesOnly_False', () => {
        const settings: SearchSettings = getSettings();
        settings.archivesOnly = true;
        const searcher: Searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.filterFile(file)).toBeFalsy();
    });

    /*************************************************************
     * searchLines test
     *************************************************************/
    it('TestSearchLines', () => {
        const settings: SearchSettings = getSettings();
        const searcher: Searcher = new Searcher(settings);
        const lines: string[] = FileUtil.getFileLines(testFile, 'utf-8');

        const results: SearchResult[] = searcher.searchLines(lines);
        expect(results.length).toEqual(2);

        const firstResult: SearchResult = results[0];
        const expectedFirstLineNum = 29;
        expect(firstResult.linenum).toEqual(expectedFirstLineNum);
        const expectedFirstMatchStartIndex = 3;
        expect(firstResult.matchStartIndex).toEqual(expectedFirstMatchStartIndex);
        const expectedFirstMatchEndIndex = 11;
        expect(firstResult.matchEndIndex).toEqual(expectedFirstMatchEndIndex);

        const secondResult: SearchResult = results[1];
        const expectedSecondLineNum = 35;
        expect(secondResult.linenum).toEqual(expectedSecondLineNum);
        const expectedSecondMatchStartIndex = 24;
        expect(secondResult.matchStartIndex).toEqual(expectedSecondMatchStartIndex);
        const expectedSecondMatchEndIndex = 32;
        expect(secondResult.matchEndIndex).toEqual(expectedSecondMatchEndIndex);
    });

    /*************************************************************
     * searchMultiLineString test
     *************************************************************/
    it('TestSearchMultiLineString', () => {
        const settings: SearchSettings = getSettings();
        const searcher: Searcher = new Searcher(settings);
        const contents: string = FileUtil.getFileContents(testFile, 'utf-8');

        const results: SearchResult[] = searcher.searchMultiLineString(contents);
        expect(results.length).toEqual(2);

        const firstResult: SearchResult = results[0];
        const expectedFirstLineNum = 29;
        expect(firstResult.linenum).toEqual(expectedFirstLineNum);
        const expectedFirstMatchStartIndex = 3;
        expect(firstResult.matchStartIndex).toEqual(expectedFirstMatchStartIndex);
        const expectedFirstMatchEndIndex = 11;
        expect(firstResult.matchEndIndex).toEqual(expectedFirstMatchEndIndex);

        const secondResult: SearchResult = results[1];
        const expectedSecondLineNum = 35;
        expect(secondResult.linenum).toEqual(expectedSecondLineNum);
        const expectedSecondMatchStartIndex = 24;
        expect(secondResult.matchStartIndex).toEqual(expectedSecondMatchStartIndex);
        const expectedSecondMatchEndIndex = 32;
        expect(secondResult.matchEndIndex).toEqual(expectedSecondMatchEndIndex);
    });
});
