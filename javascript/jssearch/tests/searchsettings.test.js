/*
 * searchsettings.test.js
 *
 * Some tests of searchsettings.js
 */

const SearchSettings = require('../src/searchsettings').SearchSettings;

describe('testing searchsettings', () => {
    it('testDefaultSettings', () => {
        const settings = new SearchSettings();
        expect(settings.archivesOnly).toBeFalsy();
        expect(settings.debug).toBeFalsy();
        expect(settings.excludeHidden).toBeTruthy();
        expect(settings.firstMatch).toBeFalsy();
        expect(settings.linesAfter).toEqual(0);
        expect(settings.linesBefore).toEqual(0);
        expect(settings.listDirs).toBeFalsy();
        expect(settings.listFiles).toBeFalsy();
        expect(settings.listLines).toBeFalsy();
        expect(settings.maxLineLength).toEqual(150);
        expect(settings.multilineSearch).toBeFalsy();
        expect(settings.printResults).toBeFalsy();
        expect(settings.printUsage).toBeFalsy();
        expect(settings.printVersion).toBeFalsy();
        expect(settings.recursive).toBeTruthy();
        expect(settings.searchArchives).toBeFalsy();
        expect(settings.uniqueLines).toBeFalsy();
        expect(settings.verbose).toBeFalsy();
    });

    it('testAddExtensionsAsCommaSeparatedString', () => {
        let settings = new SearchSettings();
        settings.addInExtensions("js,java");
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.inExtensions[0]).toEqual('js');
        expect(settings.inExtensions[1]).toEqual('java');
    });

    it('testAddExtensionsAsArray', () => {
        let settings = new SearchSettings();
        settings.addInExtensions(["js","java"]);
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.inExtensions[0]).toEqual('js');
        expect(settings.inExtensions[1]).toEqual('java');
    });

    it('testAddSearchPattern', () => {
        let settings = new SearchSettings();
        settings.addSearchPatterns("Searcher");
        expect(settings.searchPatterns.length).toEqual(1);
        expect(settings.searchPatterns[0].source).toEqual('Searcher');
    });

    it('testAddSearchPatterns', () => {
        let settings = new SearchSettings();
        settings.addSearchPatterns(["Searcher", "FileTypes"]);
        expect(settings.searchPatterns.length).toEqual(2);
        expect(settings.searchPatterns[0].source).toEqual('Searcher');
        expect(settings.searchPatterns[1].source).toEqual('FileTypes');
    });

    it('testSetArchivesOnly', () => {
        let settings = new SearchSettings();
        expect(settings.archivesOnly).toBeFalsy();
        expect(settings.searchArchives).toBeFalsy();
        settings.setArchivesOnly();
        expect(settings.archivesOnly).toBeTruthy();
        expect(settings.searchArchives).toBeTruthy();
    });

    it('testSetDebug', () => {
        let settings = new SearchSettings();
        expect(settings.debug).toBeFalsy();
        expect(settings.verbose).toBeFalsy();
        settings.setDebug();
        expect(settings.debug).toBeTruthy();
        expect(settings.verbose).toBeTruthy();
    });
});
