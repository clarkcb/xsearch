/*
 * searchoptions.test.js
 *
 * Some tests of searchoptions.js
 */

import {SearchOptions} from '../src/searchoptions';
import {SearchSettings} from '../src/searchsettings';

describe('testing searchoptions', () => {
    it('testNoArgs', () => {
        const searchOptions: SearchOptions = new SearchOptions();
        searchOptions.settingsFromArgs([], function (err: Error | void, settings: SearchSettings) {
            if (err) {
                console.log("There was an error calling settingsFromArgs: " + err);
                expect(false).toEqual(true);
            }
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
            expect(settings.printResults).toBeTruthy();
            expect(settings.printUsage).toBeFalsy();
            expect(settings.printVersion).toBeFalsy();
            expect(settings.recursive).toBeTruthy();
            expect(settings.searchArchives).toBeFalsy();
            expect(settings.paths.length).toEqual(0);
            expect(settings.uniqueLines).toBeFalsy();
            expect(settings.verbose).toBeFalsy();
        });
    });

    it('testValidArgs', () => {
        const searchOptions: SearchOptions = new SearchOptions();
        const args: string[] = ['-x', 'js,java', '-s', 'Searcher', '.'];
        searchOptions.settingsFromArgs(args, function (err: Error | void, settings: SearchSettings) {
            if (err) {
                console.log("There was an error calling settingsFromArgs: " + err);
                expect(false).toEqual(true);
            }
            expect(settings.inExtensions.length).toEqual(2);
            expect(settings.inExtensions[0]).toEqual('js');
            expect(settings.inExtensions[1]).toEqual('java');
            expect(settings.searchPatterns.length).toEqual(1);
            expect(settings.searchPatterns[0].source).toEqual('Searcher');
            expect(settings.paths.length).toEqual(1);
            expect(settings.paths[0]).toEqual('.');
        });
    });

    it('testInvalidArg', () => {
        const searchOptions: SearchOptions = new SearchOptions();
        const args: string[] = ['-Q'];
        searchOptions.settingsFromArgs(args, function(err: Error|void) {
            if (err) {
                const expected = "Invalid option: Q";
                expect(err.message).toEqual(expected);
            } else {
                expect(false).toEqual(true);
            }
        });
    });

    it('testSettingsFromJson', () => {
        const searchOptions: SearchOptions = new SearchOptions();
        const settings: SearchSettings = new SearchSettings();
        const json: string = '{\n' +
            '  "path": "~/src/xsearch/",\n' +
            '  "in-ext": ["js","ts"],\n' +
            '  "out-dirpattern": "node_module",\n' +
            '  "out-filepattern": ["temp"],\n' +
            '  "searchpattern": "Searcher",\n' +
            '  "linesbefore": 2,\n' +
            '  "linesafter": 2,\n' +
            '  "debug": true,\n' +
            '  "allmatches": false,\n' +
            '  "includehidden": true\n' +
            '}';
        const err: Error|void = searchOptions.settingsFromJson(json, settings);
        expect(err).toBeUndefined();
        expect(settings.paths.length).toEqual(1);
        expect(settings.paths[0]).toEqual('~/src/xsearch/');
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.outDirPatterns.length).toEqual(1);
        expect(settings.outDirPatterns[0].source).toEqual('node_module');
        expect(settings.outFilePatterns.length).toEqual(1);
        expect(settings.outFilePatterns[0].source).toEqual('temp');
        expect(settings.searchPatterns.length).toEqual(1);
        expect(settings.searchPatterns[0].source).toEqual('Searcher');
        expect(settings.linesBefore).toEqual(2);
        expect(settings.linesAfter).toEqual(2);
        expect(settings.debug).toBeTruthy();
        expect(settings.verbose).toBeTruthy();
        expect(settings.firstMatch).toBeTruthy();
        expect(!settings.excludeHidden).toBeTruthy();
    });
});
