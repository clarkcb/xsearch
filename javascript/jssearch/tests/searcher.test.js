/*
 * searcher_test.js
 *
 * Some tests of searcher.js
 */

const config = require('../src/config');
const FileUtil = require('../src/fileutil');
const Searcher = require('../src/searcher').Searcher;
const SearchSettings = require('../src/searchsettings').SearchSettings;

const testFile = config.SHAREDPATH + "/testFiles/testFile2.txt";

const getSettings = () => {
    let settings = new SearchSettings();
    settings.startPath = '.';
    settings.addSearchPatterns('Searcher');
    return settings;
};

describe('testing searcher', () => {
    /*************************************************************
     * isSearchDir tests
     *************************************************************/
    it('testisSearchDir_SingleDot_True', () => {
        const settings = getSettings();
        const searcher = new Searcher(settings);
        expect(searcher.isSearchDir(".")).toBeTruthy();
    });

    it('testisSearchDir_DoubleDot_True', () => {
        const settings = getSettings();
        const searcher = new Searcher(settings);
        expect(searcher.isSearchDir("..")).toBeTruthy();
    });

    it('testisSearchDir_IsHidden_False', () => {
        const settings = getSettings();
        const searcher = new Searcher(settings);
        expect(searcher.isSearchDir(".git")).toBeFalsy();
    });

    it('testisSearchDir_IsHiddenIncludeHidden_True', () => {
        const settings = getSettings();
        settings.excludeHidden = false;
        const searcher = new Searcher(settings);
        expect(searcher.isSearchDir(".git")).toBeTruthy();
    });

    it('testisSearchDir_NoPatterns_True', () => {
        const settings = getSettings();
        const searcher = new Searcher(settings);
        expect(searcher.isSearchDir("/Users")).toBeTruthy();
    });

    it('testisSearchDir_MatchesInPattern_True', () => {
        const settings = getSettings();
        settings.addInDirPatterns("Search");
        const searcher = new Searcher(settings);
        expect(searcher.isSearchDir("CsSearch")).toBeTruthy();
    });

    it('testisSearchDir_MatchesOutPattern_False', () => {
        const settings = getSettings();
        settings.addOutDirPatterns("Search");
        const searcher = new Searcher(settings);
        expect(searcher.isSearchDir("CsSearch")).toBeFalsy();
    });

    it('testisSearchDir_DoesNotMatchInPattern_False', () => {
        const settings = getSettings();
        settings.addInDirPatterns("SearchFiles");
        const searcher = new Searcher(settings);
        expect(searcher.isSearchDir("CsSearch")).toBeFalsy();
    });

    it('testisSearchDir_DoesNotMatchOutPattern_True', () => {
        const settings = getSettings();
        settings.addOutDirPatterns("SearchFiles");
        const searcher = new Searcher(settings);
        const dir = "CsSearch";
        expect(searcher.isSearchDir(dir)).toBeTruthy();
    });

    /*************************************************************
     * isSearchFile tests
     *************************************************************/
    it('testIsSearchFile_NoExtensionsNoPatterns_True', () => {
        const settings = getSettings();
        const searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.isSearchFile(file)).toBeTruthy();
    });

    it('testIsSearchFile_MatchesInExtension_True', () => {
        const settings = getSettings();
        settings.addInExtensions("cs");
        const searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.isSearchFile(file)).toBeTruthy();
    });

    it('testIsSearchFile_DoesNotMatchInExtension_False', () => {
        const settings = getSettings();
        settings.addInExtensions("java");
        const searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.isSearchFile(file)).toBeFalsy();
    });

    it('testIsSearchFile_MatchesOutExtension_False', () => {
        const settings = getSettings();
        settings.addOutExtensions("cs");
        const searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.isSearchFile(file)).toBeFalsy();
    });

    it('testIsSearchFile_DoesNotMatchOutExtension_True', () => {
        const settings = getSettings();
        settings.addOutExtensions("java");
        const searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.isSearchFile(file)).toBeTruthy();
    });

    it('testIsSearchFile_MatchesInPattern_True', () => {
        const settings = getSettings();
        settings.addInFilePatterns("Search");
        const searcher = new Searcher(settings);
        const file = "Searcher.cs";
        expect(searcher.isSearchFile(file)).toBeTruthy();
    });

    it('testIsSearchFile_DoesNotMatchInPattern_False', () => {
        const settings = getSettings();
        settings.addInFilePatterns("Search");
        const searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.isSearchFile(file)).toBeFalsy();
    });

    it('testIsSearchFile_MatchesOutPattern_False', () => {
        const settings = getSettings();
        settings.addOutFilePatterns("Search");
        const searcher = new Searcher(settings);
        const file = "Searcher.cs";
        expect(searcher.isSearchFile(file)).toBeFalsy();
    });

    it('testIsSearchFile_DoesNotMatchOutPattern_True', () => {
        const settings = getSettings();
        settings.addOutFilePatterns("Search");
        const searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.isSearchFile(file)).toBeTruthy();
    });

    /*************************************************************
     * IsArchiveSearchFile tests
     *************************************************************/
    it('testIsArchiveSearchFile_NoExtensionsNoPatterns_True', () => {
        const settings = getSettings();
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeTruthy();
    });

    it('testIsArchiveSearchFile_MatchesInExtension_True', () => {
        const settings = getSettings();
        settings.addInArchiveExtensions("zip");
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeTruthy();
    });

    it('testIsArchiveSearchFile_DoesNotMatchInExtension_False', () => {
        const settings = getSettings();
        settings.addInArchiveExtensions("gz");
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeFalsy();
    });

    it('testIsArchiveSearchFile_MatchesOutExtension_False', () => {
        const settings = getSettings();
        settings.addOutArchiveExtensions("zip");
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeFalsy();
    });

    it('testIsArchiveSearchFile_DoesNotMatchOutExtension_True', () => {
        const settings = getSettings();
        settings.addOutArchiveExtensions("gz");
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeTruthy();
    });

    it('testIsArchiveSearchFile_MatchesInPattern_True', () => {
        const settings = getSettings();
        settings.addInArchiveFilePatterns("arch");
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeTruthy();
    });

    it('testIsArchiveSearchFile_DoesNotMatchInPattern_False', () => {
        const settings = getSettings();
        settings.addInArchiveFilePatterns("archives");
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeFalsy();
    });

    it('testIsArchiveSearchFile_MatchesOutPattern_False', () => {
        const settings = getSettings();
        settings.addOutArchiveFilePatterns("arch");
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeFalsy();
    });

    it('testIsArchiveSearchFile_DoesNotMatchOutPattern_True', () => {
        const settings = getSettings();
        settings.addOutArchiveFilePatterns("archives");
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.isArchiveSearchFile(file)).toBeTruthy();
    });

    /*************************************************************
     * filterFile tests
     *************************************************************/
    it('testFilterFile_IsHidden_False', () => {
        const settings = getSettings();
        const searcher = new Searcher(settings);
        const file = ".gitignore";
        expect(searcher.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_IsHiddenIncludeHidden_True', () => {
        const settings = getSettings();
        settings.excludeHidden = false;
        const searcher = new Searcher(settings);
        const file = ".gitignore";
        expect(searcher.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_ArchiveNoSearchArchives_False', () => {
        const settings = getSettings();
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_ArchiveSearchArchives_True', () => {
        const settings = getSettings();
        settings.searchArchives = true;
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_IsArchiveSearchFile_True', () => {
        const settings = getSettings();
        settings.searchArchives = true;
        settings.addInArchiveExtensions("zip");
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_NotIsArchiveSearchFile_False', () => {
        const settings = getSettings();
        settings.addOutExtensions("zip");
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_ArchiveFileArchivesOnly_True', () => {
        const settings = getSettings();
        settings.archivesOnly = true;
        const searcher = new Searcher(settings);
        const file = "archive.zip";
        expect(searcher.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_NoExtensionsNoPatterns_True', () => {
        const settings = getSettings();
        const searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_IsSearchFile_True', () => {
        const settings = getSettings();
        settings.addInExtensions("cs");
        const searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_NotIsSearchFile_False', () => {
        const settings = getSettings();
        settings.addOutExtensions("cs");
        const searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_NonArchiveFileArchivesOnly_False', () => {
        const settings = getSettings();
        settings.archivesOnly = true;
        const searcher = new Searcher(settings);
        const file = "FileUtil.cs";
        expect(searcher.filterFile(file)).toBeFalsy();
    });

    /*************************************************************
     * searchLines test
     *************************************************************/
    it('TestSearchLines', async () => {
        const settings = getSettings();
        const searcher = new Searcher(settings);
        const lines = FileUtil.getFileLines(testFile);

        const results = await searcher.searchLines(lines);
        expect(results.length).toEqual(2);

        const firstResult = results[0];
        const expectedFirstLineNum = 29;
        expect(firstResult.linenum).toEqual(expectedFirstLineNum);
        const expectedFirstMatchStartIndex = 3;
        expect(firstResult.matchStartIndex).toEqual(expectedFirstMatchStartIndex);
        const expectedFirstMatchEndIndex = 11;
        expect(firstResult.matchEndIndex).toEqual(expectedFirstMatchEndIndex);

        const secondResult = results[1];
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
        const settings = getSettings();
        const searcher = new Searcher(settings);
        const contents = FileUtil.getFileContents(testFile);

        searcher.searchMultiLineString(contents, (err, results) => {
            expect(results.length).toEqual(2);

            const firstResult = results[0];
            const expectedFirstLineNum = 29;
            expect(firstResult.linenum).toEqual(expectedFirstLineNum);
            const expectedFirstMatchStartIndex = 3;
            expect(firstResult.matchStartIndex).toEqual(expectedFirstMatchStartIndex);
            const expectedFirstMatchEndIndex = 11;
            expect(firstResult.matchEndIndex).toEqual(expectedFirstMatchEndIndex);

            const secondResult = results[1];
            const expectedSecondLineNum = 35;
            expect(secondResult.linenum).toEqual(expectedSecondLineNum);
            const expectedSecondMatchStartIndex = 24;
            expect(secondResult.matchStartIndex).toEqual(expectedSecondMatchStartIndex);
            const expectedSecondMatchEndIndex = 32;
            expect(secondResult.matchEndIndex).toEqual(expectedSecondMatchEndIndex);
        });
    });
});
