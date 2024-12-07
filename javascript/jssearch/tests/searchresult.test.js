/*
 * searchresult.test.js
 *
 * Some tests of searchresult.js
 */

const {COLORS} = require('../src/color');
const config = require('../src/config');
const {FileResult, FileType} = require('jsfind');
const {SearchResult} = require('../src/searchresult');
const {SearchResultFormatter} = require('../src/searchresultformatter');
const {SearchSettings} = require('../src/searchsettings');

const cssearch_path = config.XSEARCH_PATH + '/csharp/CsSearch/CsSearch';

describe('testing searchresult', () => {
    it('testSingleLineResult', () => {
        const settings = new SearchSettings();
        settings.colorize = false;
        const formatter = new SearchResultFormatter(settings);
        const pattern = 'Search';
        const file = new FileResult(cssearch_path, 'Searcher.cs', FileType.CODE, null);
        const lineNum = 10;
        const matchStartIndex = 15;
        const matchEndIndex = 23;
        const line = "\tpublic class Searcher\n";
        const linesBefore = [];
        const linesAfter = [];
        const result = new SearchResult(pattern, file, lineNum, matchStartIndex,
            matchEndIndex, line, linesBefore, linesAfter);
        const expectedOutput = file + ": " + lineNum + ": [" + matchStartIndex + ":" +
            matchEndIndex + "]: " + line.trim();
        const output = formatter.format(result)
        expect(output).toEqual(expectedOutput);
    });

    it('testSingleLineLongerThanMaxLengthResult', () => {
        const settings = new SearchSettings();
        settings.colorize = false;
        settings.maxLineLength = 100;
        const formatter = new SearchResultFormatter(settings);
        const pattern = 'maxlen';
        const file = new FileResult(cssearch_path, 'maxlen.txt', FileType.TEXT, null);
        const lineNum = 1;
        const matchStartIndex = 53;
        const matchEndIndex = 59;
        // This line is 110 chars long
        const line = '0123456789012345678901234567890123456789012345678901' +
            'maxlen' +
            '8901234567890123456789012345678901234567890123456789';
        const linesBefore = [];
        const linesAfter = [];
        const result = new SearchResult(pattern, file, lineNum, matchStartIndex,
            matchEndIndex, line, linesBefore, linesAfter);
        const expectedLine = '...89012345678901234567890123456789012345678901' +
            'maxlen' +
            '89012345678901234567890123456789012345678901...';
        const expectedOutput = file + ': ' + lineNum + ': [' + matchStartIndex + ':' +
            matchEndIndex + ']: ' + expectedLine;
        const output = formatter.format(result)
        expect(output).toEqual(expectedOutput);
    });

    it('testSingleLineLongerColorizeResult', () => {
        const settings = new SearchSettings();
        settings.maxLineLength = 100;
        const formatter = new SearchResultFormatter(settings);
        const pattern = 'maxlen';
        const file = new FileResult(cssearch_path, 'maxlen.txt', FileType.TEXT, null);
        const lineNum = 1;
        const matchStartIndex = 53;
        const matchEndIndex = 59;
        // This line is 110 chars long
        const line = '0123456789012345678901234567890123456789012345678901' +
            'maxlen' +
            '8901234567890123456789012345678901234567890123456789';
        const linesBefore = [];
        const linesAfter = [];
        const result = new SearchResult(pattern, file, lineNum, matchStartIndex,
            matchEndIndex, line, linesBefore, linesAfter);
        const expectedLine = '...89012345678901234567890123456789012345678901' +
            COLORS.GREEN +
            'maxlen' +
            COLORS.RESET +
            '89012345678901234567890123456789012345678901...';
        const expectedOutput = file + ': ' + lineNum + ': [' + matchStartIndex + ':' +
            matchEndIndex + ']: ' + expectedLine;
        const output = formatter.format(result)
        expect(output).toEqual(expectedOutput);
    });

    it('testBinaryFileResult', () => {
        const settings = new SearchSettings();
        const formatter = new SearchResultFormatter(settings);
        const pattern = 'Search';
        const file = new FileResult(cssearch_path + '/bin/Debug', 'CsSearch.exe', FileType.BINARY, null);
        const lineNum = 0;
        const matchStartIndex = 5;
        const matchEndIndex = 10;
        const line = "";
        const linesBefore = [];
        const linesAfter = [];
        const result = new SearchResult(pattern, file, lineNum, matchStartIndex,
            matchEndIndex, line, linesBefore, linesAfter);
        const expectedOutput = file + ' matches at [5:10]';
        const output = formatter.format(result);
        expect(output).toEqual(expectedOutput);
    });

    it('testMultiLineResult', () => {
        const settings = new SearchSettings();
        settings.colorize = false;
        const formatter = new SearchResultFormatter(settings);
        const pattern = 'Search';
        const file = new FileResult(cssearch_path, 'Searcher.cs', FileType.CODE, null);
        const lineNum = 10;
        const matchStartIndex = 15;
        const matchEndIndex = 23;
        const line = "\tpublic class Searcher\n";
        const linesBefore = ["namespace CsSearch\n", "{\n"];
        const linesAfter = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"];
        const result = new SearchResult(pattern, file, lineNum, matchStartIndex,
            matchEndIndex, line, linesBefore, linesAfter);
        const expectedOutput = '' +
            "================================================================================\n" +
            `${file}: ${lineNum}: [${matchStartIndex}:${matchEndIndex}]\n` +
            "--------------------------------------------------------------------------------\n" +
            "   8 | namespace CsSearch\n" +
            "   9 | {\n" +
            "> 10 | \tpublic class Searcher\n" +
            "  11 | \t{\n" +
            "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
        const output = formatter.format(result);
        expect(output).toEqual(expectedOutput);
    });
});
