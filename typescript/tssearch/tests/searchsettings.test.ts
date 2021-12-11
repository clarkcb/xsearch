/*
 * searchsettings.test.js
 *
 * Some tests of searchsettings.js
 */

import {SearchSettings} from '../src/searchsettings';

describe('testing searchsettings', () => {
    it('testDefaultSettings', () => {
        const settings: SearchSettings = new SearchSettings();
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
        expect(settings.paths.length).toEqual(0);
        expect(settings.uniqueLines).toBeFalsy();
        expect(settings.verbose).toBeFalsy();
    });

    it('testAddExtensionsAsCommaSeparatedString', () => {
        const settings: SearchSettings = new SearchSettings();
        settings.addInExtensions("js,java");
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.inExtensions[0]).toEqual('js');
        expect(settings.inExtensions[1]).toEqual('java');
    });

    it('testAddExtensionsAsArray', () => {
        const settings: SearchSettings = new SearchSettings();
        settings.addInExtensions(["js","java"]);
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.inExtensions[0]).toEqual('js');
        expect(settings.inExtensions[1]).toEqual('java');
    });

    it('testAddSearchPattern', () => {
        const settings: SearchSettings = new SearchSettings();
        settings.addSearchPatterns("Searcher");
        expect(settings.searchPatterns.length).toEqual(1);
        expect(settings.searchPatterns[0].source).toEqual('Searcher');
    });

    it('testAddSearchPatterns', () => {
        const settings: SearchSettings = new SearchSettings();
        settings.addSearchPatterns(["Searcher", "FileTypes"]);
        expect(settings.searchPatterns.length).toEqual(2);
        expect(settings.searchPatterns[0].source).toEqual('Searcher');
        expect(settings.searchPatterns[1].source).toEqual('FileTypes');
    });

    it('testSetArchivesOnly', () => {
        const settings: SearchSettings = new SearchSettings();
        expect(settings.archivesOnly).toBeFalsy();
        expect(settings.searchArchives).toBeFalsy();
        settings.setArchivesOnly(true);
        expect(settings.archivesOnly).toBeTruthy();
        expect(settings.searchArchives).toBeTruthy();
    });

    it('testSetDebug', () => {
        const settings: SearchSettings = new SearchSettings();
        expect(settings.debug).toBeFalsy();
        expect(settings.verbose).toBeFalsy();
        settings.setDebug(true);
        expect(settings.debug).toBeTruthy();
        expect(settings.verbose).toBeTruthy();
    });
});
