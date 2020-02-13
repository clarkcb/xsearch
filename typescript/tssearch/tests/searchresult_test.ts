/*
 * searchresult_test.js
 *
 * Some nodeunit tests of searchresult.js
 */

import * as config from '../src/config';
import {SearchResult} from '../src/searchresult';
import {SearchFile} from '../src/searchfile';
import {FileType} from '../src/filetype';

const cssearch_path = config.XSEARCHPATH + '/csharp/CsSearch/CsSearch';

exports.testSingleLineResult = function(test) {
    const pattern: RegExp = /Search/;
    const linenum: number = 10;
    const matchStartIndex: number = 15;
    const matchEndIndex: number = 23;
    const line: string = "\tpublic class Searcher\n";
    const linesBefore: string[] = [];
    const linesAfter: string[] = [];
    let result: SearchResult = new SearchResult(pattern, linenum, matchStartIndex, matchEndIndex,
        line, linesBefore, linesAfter);
    const searchfile = new SearchFile(cssearch_path, 'Searcher.cs', FileType.Code);
    result.file = searchfile;
    const resultString: string = result.toString();
    const expectedOutput: string = searchfile.toString() + ": " + linenum + ": [" + matchStartIndex + ":" +
        matchEndIndex + "]: " + line.trim();
    test.ok(resultString === expectedOutput, "single-line result matches expected");
    test.done();
};

exports.testBinaryFileResult = function(test) {
    const pattern: RegExp = /Search/;
    const linenum: number = 0;
    const matchStartIndex: number = 5;
    const matchEndIndex: number = 10;
    const line: string = "";
    const linesBefore: string[] = [];
    const linesAfter: string[] = [];
    let result: SearchResult = new SearchResult(pattern, linenum, matchStartIndex, matchEndIndex,
        line, linesBefore, linesAfter);
    const searchfile = new SearchFile(cssearch_path + 'bin/Debug', 'CsSearch.exe', FileType.Binary);
    result.file = searchfile;
    const resultString: string = result.toString();
    const expectedOutput: string = searchfile.toString() + " matches at [5:10]";
    test.ok(resultString === expectedOutput, "single-line result matches expected");
    test.done();
};

exports.testMultiLineResult = function(test) {
    const pattern: RegExp = /Search/;
    const linenum: number = 10;
    const matchStartIndex: number = 15;
    const matchEndIndex: number = 23;
    const line: string = "\tpublic class Searcher\n";
    const linesBefore: string[] = ["namespace CsSearch\n", "{\n"];
    const linesAfter: string[] = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"];
    let result: SearchResult = new SearchResult(pattern, linenum, matchStartIndex,
        matchEndIndex, line, linesBefore, linesAfter);
    const searchfile = new SearchFile(cssearch_path, 'Searcher.cs', FileType.Code);
    result.file = searchfile;
    const resultString: string = result.toString();
    const expectedOutput = "" +
        "================================================================================\n" +
        `${searchfile.toString()}: ${linenum}: [${matchStartIndex}:${matchEndIndex}]\n` +
        "--------------------------------------------------------------------------------\n" +
        "   8 | namespace CsSearch\n" +
        "   9 | {\n" +
        "> 10 | \tpublic class Searcher\n" +
        "  11 | \t{\n" +
        "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
    test.ok(resultString === expectedOutput, "multi-line result matches expected");
    test.done();
};
