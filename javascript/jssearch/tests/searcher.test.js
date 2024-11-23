/*
 * searcher_test.js
 *
 * Some tests of searcher.js
 */

const config = require('../src/config');
const {FileUtil} = require('jsfind');
const {Searcher} = require('../src/searcher');
const {SearchSettings} = require('../src/searchsettings');

const testFile = config.SHAREDPATH + "/testFiles/testFile2.txt";

const getSettings = () => {
    let settings = new SearchSettings();
    settings.paths = ['.'];
    settings.addSearchPatterns('Searcher');
    return settings;
};

describe('testing searcher', () => {
    /*************************************************************
     * searchLines test
     *************************************************************/
    it('TestSearchLines', async () => {
        const settings = getSettings();
        const searcher = new Searcher(settings);
        const lines = FileUtil.getFileLinesSync(testFile);

        const results = await searcher.searchLines(lines);
        expect(results.length).toEqual(2);

        const firstResult = results[0];
        const expectedFirstLineNum = 30;
        expect(firstResult.lineNum).toEqual(expectedFirstLineNum);
        const expectedFirstMatchStartIndex = 3;
        expect(firstResult.matchStartIndex).toEqual(expectedFirstMatchStartIndex);
        const expectedFirstMatchEndIndex = 11;
        expect(firstResult.matchEndIndex).toEqual(expectedFirstMatchEndIndex);

        const secondResult = results[1];
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
    it('TestSearchMultiLineString', () => {
        const settings = getSettings();
        const searcher = new Searcher(settings);
        const contents = FileUtil.getFileContents(testFile);

        searcher.searchMultiLineString(contents, (err, results) => {
            expect(results.length).toEqual(2);

            const firstResult = results[0];
            const expectedFirstLineNum = 30;
            expect(firstResult.lineNum).toEqual(expectedFirstLineNum);
            const expectedFirstMatchStartIndex = 3;
            expect(firstResult.matchStartIndex).toEqual(expectedFirstMatchStartIndex);
            const expectedFirstMatchEndIndex = 11;
            expect(firstResult.matchEndIndex).toEqual(expectedFirstMatchEndIndex);

            const secondResult = results[1];
            const expectedSecondLineNum = 36;
            expect(secondResult.lineNum).toEqual(expectedSecondLineNum);
            const expectedSecondMatchStartIndex = 24;
            expect(secondResult.matchStartIndex).toEqual(expectedSecondMatchStartIndex);
            const expectedSecondMatchEndIndex = 32;
            expect(secondResult.matchEndIndex).toEqual(expectedSecondMatchEndIndex);
        });
    });
});
