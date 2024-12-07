/*
 * searcher.test.js
 *
 * Some tests of searcher.js
 */

import * as config from '../src/config';
import {FileUtil} from 'tsfind';
import {Searcher} from '../src/searcher';
import {SearchResult} from '../src/searchresult';
import {SearchSettings} from '../src/searchsettings';

const testFile = config.SHARED_PATH + '/testFiles/testFile2.txt';

const getSettings = function() {
    const settings: SearchSettings = new SearchSettings();
    settings.addSearchPatterns('Searcher');
    settings.paths.push('.');
    return settings;
};

describe('testing searcher', () => {
    /*************************************************************
     * searchLines test
     *************************************************************/
    it('TestSearchLines', async () => {
        const settings: SearchSettings = getSettings();
        const searcher: Searcher = new Searcher(settings);
        const lines: string[] = FileUtil.getFileLinesSync(testFile, 'utf-8');

        const results: SearchResult[] = await searcher.searchLines(lines);
        expect(results.length).toEqual(2);

        const firstResult: SearchResult = results[0];
        const expectedFirstLineNum = 30;
        expect(firstResult.lineNum).toEqual(expectedFirstLineNum);
        const expectedFirstMatchStartIndex = 3;
        expect(firstResult.matchStartIndex).toEqual(expectedFirstMatchStartIndex);
        const expectedFirstMatchEndIndex = 11;
        expect(firstResult.matchEndIndex).toEqual(expectedFirstMatchEndIndex);

        const secondResult: SearchResult = results[1];
        const expectedSecondLineNum = 36;
        expect(secondResult.lineNum).toEqual(expectedSecondLineNum);
        const expectedSecondMatchStartIndex = 24;
        expect(secondResult.matchStartIndex).toEqual(expectedSecondMatchStartIndex);
        const expectedSecondMatchEndIndex = 32;
        expect(secondResult.matchEndIndex).toEqual(expectedSecondMatchEndIndex);
    });

    /*************************************************************
     * searchMultiLineString test
     *************************************************************/
    it('TestSearchMultiLineString', async () => {
        const settings: SearchSettings = getSettings();
        const searcher: Searcher = new Searcher(settings);
        const contents: string = FileUtil.getFileContentsSync(testFile, 'utf-8');

        const results: SearchResult[] = await searcher.searchMultiLineString(contents);
        expect(results.length).toEqual(2);

        const firstResult: SearchResult = results[0];
        const expectedFirstLineNum = 30;
        expect(firstResult.lineNum).toEqual(expectedFirstLineNum);
        const expectedFirstMatchStartIndex = 3;
        expect(firstResult.matchStartIndex).toEqual(expectedFirstMatchStartIndex);
        const expectedFirstMatchEndIndex = 11;
        expect(firstResult.matchEndIndex).toEqual(expectedFirstMatchEndIndex);

        const secondResult: SearchResult = results[1];
        const expectedSecondLineNum = 36;
        expect(secondResult.lineNum).toEqual(expectedSecondLineNum);
        const expectedSecondMatchStartIndex = 24;
        expect(secondResult.matchStartIndex).toEqual(expectedSecondMatchStartIndex);
        const expectedSecondMatchEndIndex = 32;
        expect(secondResult.matchEndIndex).toEqual(expectedSecondMatchEndIndex);
    });
});
