/*
 * searchoptions_test.js
 *
 * Some tests of searchoptions.js
 */

const {SearchOptions} = require('../src/searchoptions');
const {SearchSettings} = require('../src/searchsettings');

describe('testing searchoptions', () => {
    it('testNoArgs', () => {
        const searchOptions = new SearchOptions();
        searchOptions.settingsFromArgs([], (err, settings) => {
            if (err) {
                console.log("There was an error calling settingsFromArgs: " + err);
                expect(false).toEqual(true);
            }
            expect(settings.archivesOnly).toBeFalsy();
            expect(settings.debug).toBeFalsy();
            expect(settings.firstMatch).toBeFalsy();
            expect(settings.followSymlinks).toBeFalsy();
            expect(settings.includeHidden).toBeFalsy();
            expect(settings.includeArchives).toBeFalsy();
            expect(settings.linesAfter).toEqual(0);
            expect(settings.linesBefore).toEqual(0);
            expect(settings.printDirs).toBeFalsy();
            expect(settings.printFiles).toBeFalsy();
            expect(settings.printLines).toBeFalsy();
            expect(settings.maxLineLength).toEqual(150);
            expect(settings.maxSize).toEqual(0);
            expect(settings.minSize).toEqual(0);
            expect(settings.multilineSearch).toBeFalsy();
            expect(settings.printResults).toBeTruthy();
            expect(settings.printUsage).toBeFalsy();
            expect(settings.printVersion).toBeFalsy();
            expect(settings.recursive).toBeTruthy();
            expect(settings.searchArchives).toBeFalsy();
            expect(settings.uniqueLines).toBeFalsy();
            expect(settings.verbose).toBeFalsy();
        });
    });

    it('testValidArgs', () => {
        const searchOptions = new SearchOptions();
        const args = ['-x', 'js,java', '-s', 'Searcher', '.'];
        searchOptions.settingsFromArgs(args, (err, settings) => {
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

    it('testArchivesOnly', () => {
        const searchOptions = new SearchOptions();
        const args = ['--archivesonly'];
        searchOptions.settingsFromArgs(args, (err, settings) => {
            if (err) {
                console.log("There was an error calling settingsFromArgs: " + err);
                expect(false).toEqual(true);
            }
            expect(settings.archivesOnly).toEqual(true);
            expect(settings.includeArchives).toEqual(true);
            expect(settings.searchArchives).toEqual(true);
        });
    });

    it('testDebug', () => {
        const searchOptions = new SearchOptions();
        const args = ['--debug'];
        searchOptions.settingsFromArgs(args, (err, settings) => {
            if (err) {
                console.log("There was an error calling settingsFromArgs: " + err);
                expect(false).toEqual(true);
            }
            expect(settings.debug).toEqual(true);
            expect(settings.verbose).toEqual(true);
        });
    });

    it('testMissingArg', () => {
        const searchOptions = new SearchOptions();
        const args = ['-x'];
        searchOptions.settingsFromArgs(args, (err) => {
            if (err) {
                const expected = "Missing argument for option x";
                expect(err.message).toEqual(expected);
            } else {
                console.log("Did not get expected missing argument error");
                expect(false).toEqual(true);
            }
        });
    });

    it('testIvalidArg', () => {
        const searchOptions = new SearchOptions();
        const args = ['-Q'];
        searchOptions.settingsFromArgs(args, (err) => {
            if (err) {
                const expected = "Invalid option: Q";
                expect(err.message).toEqual(expected);
            } else {
                console.log("Did not get expected unknown option error");
                expect(false).toEqual(true);
            }
        });
    });

    it('testSettingsFromJson', () => {
        const searchOptions = new SearchOptions();
        const settings = new SearchSettings();
        const json = '{\n' +
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
        const err = searchOptions.settingsFromJson(json, settings);
        expect(err).toBeNull();
        expect(settings.inExtensions.length).toEqual(2);
        expect(settings.outDirPatterns.length).toEqual(1);
        expect(settings.outDirPatterns[0].source).toEqual('node_module');
        expect(settings.outFilePatterns.length).toEqual(1);
        expect(settings.outFilePatterns[0].source).toEqual('temp');
        expect(settings.searchPatterns.length).toEqual(1);
        expect(settings.searchPatterns[0].source).toEqual('Searcher');
        expect(settings.paths.length).toEqual(1);
        expect(settings.paths[0]).toEqual('~/src/xsearch/');
        expect(settings.linesBefore).toEqual(2);
        expect(settings.linesAfter).toEqual(2);
        expect(settings.debug).toBeTruthy();
        expect(settings.verbose).toBeTruthy();
        expect(settings.firstMatch).toBeTruthy();
        expect(settings.excludeHidden).toBeFalsy();
    });
});
