/*
 * searchresult.test.js
 *
 * Some tests of searchresult.js
 */

import {COLORS} from "../src/common";
import * as config from '../src/config';
import {FileType} from '../src/filetype';
import {SearchFile} from '../src/searchfile';
import {SearchResult} from '../src/searchresult';
import {SearchResultFormatter} from '../src/searchresultformatter';
import {SearchSettings} from '../src/searchsettings';

const cssearch_path = config.XSEARCHPATH + '/csharp/CsSearch/CsSearch';

describe('testing searchresult', () => {
    it('testSingleLineResult', () => {
        const settings = new SearchSettings();
        settings.colorize = false;
        const formatter = new SearchResultFormatter(settings);
        const pattern = /Search/;
        const linenum = 10;
        const matchStartIndex = 15;
        const matchEndIndex = 23;
        const line = "\tpublic class Searcher\n";
        const linesBefore: string[] = [];
        const linesAfter: string[] = [];
        const result: SearchResult = new SearchResult(pattern, linenum, matchStartIndex, matchEndIndex,
            line, linesBefore, linesAfter);
        const searchfile = new SearchFile(cssearch_path, 'Searcher.cs', FileType.Code);
        result.file = searchfile;
        const expectedOutput: string = searchfile.toString() + ": " + linenum + ": [" + matchStartIndex + ":" +
            matchEndIndex + "]: " + line.trim();
        const output: string = formatter.format(result)
        expect(output).toEqual(expectedOutput);
    });

    it('testSingleLineLongerThanMaxLengthResult', () => {
        const settings = new SearchSettings();
        settings.colorize = false;
        settings.maxLineLength = 100;
        const formatter = new SearchResultFormatter(settings);
        const pattern = /maxlen/;
        const linenum = 1;
        const matchStartIndex = 53;
        const matchEndIndex = 59;
        const line = '0123456789012345678901234567890123456789012345678901' +
            'maxlen' +
            '8901234567890123456789012345678901234567890123456789';
        const linesBefore: string[] = [];
        const linesAfter: string[] = [];
        const result: SearchResult = new SearchResult(pattern, linenum, matchStartIndex, matchEndIndex,
            line, linesBefore, linesAfter);
        const searchfile = new SearchFile('.', 'maxlen.txt', FileType.Text);
        result.file = searchfile;
        const expectedLine = '...89012345678901234567890123456789012345678901' +
            'maxlen' +
            '89012345678901234567890123456789012345678901...';
        const expectedOutput: string = searchfile.toString() + ": " + linenum + ": [" + matchStartIndex + ":" +
            matchEndIndex + "]: " + expectedLine;
        const output: string = formatter.format(result);
        expect(output).toEqual(expectedOutput);
    });

    it('testSingleLineLongerColorizeResult', () => {
        const settings = new SearchSettings();
        const formatter = new SearchResultFormatter(settings);
        settings.maxLineLength = 100;
        const pattern = /maxlen/;
        const linenum = 1;
        const matchStartIndex = 53;
        const matchEndIndex = 59;
        const line = '0123456789012345678901234567890123456789012345678901' +
            'maxlen' +
            '8901234567890123456789012345678901234567890123456789';
        const linesBefore: string[] = [];
        const linesAfter: string[] = [];
        const result: SearchResult = new SearchResult(pattern, linenum, matchStartIndex, matchEndIndex,
            line, linesBefore, linesAfter);
        const searchfile = new SearchFile('.', 'maxlen.txt', FileType.Text);
        result.file = searchfile;
        const expectedLine = '...89012345678901234567890123456789012345678901' +
            COLORS.GREEN +
            'maxlen' +
            COLORS.RESET +
            '89012345678901234567890123456789012345678901...';
        const expectedOutput: string = searchfile.toString() + ": " + linenum + ": [" + matchStartIndex + ":" +
            matchEndIndex + "]: " + expectedLine;
        const output: string = formatter.format(result);
        expect(output).toEqual(expectedOutput);
    });

    it('testBinaryFileResult', () => {
        const settings = new SearchSettings();
        const formatter = new SearchResultFormatter(settings);
        const pattern = /Search/;
        const linenum = 0;
        const matchStartIndex = 5;
        const matchEndIndex = 10;
        const line = "";
        const linesBefore: string[] = [];
        const linesAfter: string[] = [];
        const result: SearchResult = new SearchResult(pattern, linenum, matchStartIndex, matchEndIndex,
            line, linesBefore, linesAfter);
        const searchfile = new SearchFile(cssearch_path + 'bin/Debug', 'CsSearch.exe', FileType.Binary);
        result.file = searchfile;
        const expectedOutput: string = searchfile.toString() + " matches at [5:10]";
        const output: string = formatter.format(result);
        expect(output).toEqual(expectedOutput);
    });

    it('testMultiLineResult', () => {
        const settings = new SearchSettings();
        settings.colorize = false;
        const formatter = new SearchResultFormatter(settings);
        const pattern = /Search/;
        const linenum = 10;
        const matchStartIndex = 15;
        const matchEndIndex = 23;
        const line = "\tpublic class Searcher\n";
        const linesBefore: string[] = ["namespace CsSearch\n", "{\n"];
        const linesAfter: string[] = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"];
        const result: SearchResult = new SearchResult(pattern, linenum, matchStartIndex,
            matchEndIndex, line, linesBefore, linesAfter);
        const searchfile = new SearchFile(cssearch_path, 'Searcher.cs', FileType.Code);
        result.file = searchfile;
        const expectedOutput = '' +
            "================================================================================\n" +
            `${searchfile.toString()}: ${linenum}: [${matchStartIndex}:${matchEndIndex}]\n` +
            "--------------------------------------------------------------------------------\n" +
            "   8 | namespace CsSearch\n" +
            "   9 | {\n" +
            "> 10 | \tpublic class Searcher\n" +
            "  11 | \t{\n" +
            "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
        const output: string = formatter.format(result);
        expect(output).toEqual(expectedOutput);
    });
});
