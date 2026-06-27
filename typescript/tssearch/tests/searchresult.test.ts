/*
 * searchresult.test.js
 *
 * Some tests of searchresult.js
 */

import * as config from '../src/config';
import {FileType, FileResult, ConsoleColor} from 'tsfind';
import {SearchResult} from '../src/searchresult';
import {SearchResultFormatter} from '../src/searchresultformatter';
import {SearchSettings} from '../src/searchsettings';
import * as path from "path";

const cssearch_path = config.XSEARCH_PATH + '/csharp/CsSearch/CsSearch';

describe('testing searchresult', () => {
    it('testSingleLineResult', () => {
        const settings = new SearchSettings();
        settings.colorize = false;
        const formatter = new SearchResultFormatter(settings);
        const pattern = /Search/;
        const filePath = path.join(cssearch_path, 'Searcher.cs');
        const fileResult = new FileResult(filePath, FileType.Code, 0, 0);
        const lineNum = 10;
        const matchStartIndex = 15;
        const matchEndIndex = 23;
        const line = "\tpublic class Searcher\n";
        const linesBefore: string[] = [];
        const linesAfter: string[] = [];
        const result: SearchResult = new SearchResult(pattern, fileResult, lineNum, matchStartIndex, matchEndIndex,
            line, linesBefore, linesAfter);
        const expectedOutput: string = fileResult.toString() + ": " + lineNum + ": [" + matchStartIndex + ":" +
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
        const filePath = path.join('.', 'maxlen.txt');
        const fileResult = new FileResult(filePath, FileType.Text, 0, 0);
        const lineNum = 1;
        const matchStartIndex = 53;
        const matchEndIndex = 59;
        const line = '0123456789012345678901234567890123456789012345678901' +
            'maxlen' +
            '8901234567890123456789012345678901234567890123456789';
        const linesBefore: string[] = [];
        const linesAfter: string[] = [];
        const result: SearchResult = new SearchResult(pattern, fileResult, lineNum, matchStartIndex, matchEndIndex,
            line, linesBefore, linesAfter);
        const expectedLine = '...89012345678901234567890123456789012345678901' +
            'maxlen' +
            '89012345678901234567890123456789012345678901...';
        const expectedOutput: string = fileResult.toString() + ": " + lineNum + ": [" + matchStartIndex + ":" +
            matchEndIndex + "]: " + expectedLine;
        const output: string = formatter.format(result);
        expect(output).toEqual(expectedOutput);
    });

    it('testSingleLineLongerColorizeResult', () => {
        const settings = new SearchSettings();
        const formatter = new SearchResultFormatter(settings);
        settings.maxLineLength = 100;
        const pattern = /maxlen/;
        const filePath = path.join('.', 'maxlen.txt');
        const fileResult = new FileResult(filePath, FileType.Text, 0, 0);
        const lineNum = 1;
        const matchStartIndex = 53;
        const matchEndIndex = 59;
        const line = '0123456789012345678901234567890123456789012345678901' +
            'maxlen' +
            '8901234567890123456789012345678901234567890123456789';
        const linesBefore: string[] = [];
        const linesAfter: string[] = [];
        const result: SearchResult = new SearchResult(pattern, fileResult, lineNum, matchStartIndex, matchEndIndex,
            line, linesBefore, linesAfter);
        const expectedLine = '...89012345678901234567890123456789012345678901' +
            ConsoleColor.GREEN +
            'maxlen' +
            ConsoleColor.RESET +
            '89012345678901234567890123456789012345678901...';
        const expectedOutput: string = fileResult.toString() + ": " + lineNum + ": [" + matchStartIndex + ":" +
            matchEndIndex + "]: " + expectedLine;
        const output: string = formatter.format(result);
        expect(output).toEqual(expectedOutput);
    });

    it('testBinaryFileResult', () => {
        const settings = new SearchSettings();
        const formatter = new SearchResultFormatter(settings);
        const pattern = /Search/;
        const filePath = path.join(cssearch_path, 'bin/Debug', 'CsSearch.exe');
        const fileResult = new FileResult(filePath, FileType.Binary, 0, 0);
        const lineNum = 0;
        const matchStartIndex = 5;
        const matchEndIndex = 10;
        const line = "";
        const linesBefore: string[] = [];
        const linesAfter: string[] = [];
        const result: SearchResult = new SearchResult(pattern, fileResult, lineNum, matchStartIndex, matchEndIndex,
            line, linesBefore, linesAfter);
        const expectedOutput: string = fileResult.toString() + " matches at [5:10]";
        const output: string = formatter.format(result);
        expect(output).toEqual(expectedOutput);
    });

    it('testMultiLineResult', () => {
        const settings = new SearchSettings();
        settings.colorize = false;
        const formatter = new SearchResultFormatter(settings);
        const pattern = /Search/;
        const filePath = path.join(cssearch_path, 'Searcher.cs');
        const fileResult = new FileResult(filePath, FileType.Code, 0, 0);
        const lineNum = 10;
        const matchStartIndex = 15;
        const matchEndIndex = 23;
        const line = "\tpublic class Searcher\n";
        const linesBefore: string[] = ["namespace CsSearch\n", "{\n"];
        const linesAfter: string[] = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"];
        const result: SearchResult = new SearchResult(pattern, fileResult, lineNum, matchStartIndex,
            matchEndIndex, line, linesBefore, linesAfter);
        const expectedOutput = '' +
            "================================================================================\n" +
            `${fileResult.toString()}: ${lineNum}: [${matchStartIndex}:${matchEndIndex}]\n` +
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
