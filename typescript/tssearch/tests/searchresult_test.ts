/*
 * searchresult_test.js
 *
 * Some nodeunit tests of searchresult.js
 */

import * as config from '../src/config';
import {SearchResult} from '../src/searchresult';
import {SearchFile} from '../src/searchfile';
import {FileType} from '../src/filetype';

var cssearch_path = config.XSEARCHPATH + '/csharp/CsSearch/CsSearch';

exports.testSingleLineResult = function(test) {
    var pattern: RegExp = /Search/;
    var file: string = cssearch_path + '/Searcher.cs';
    var linenum: number = 10;
    var matchStartIndex: number = 15;
    var matchEndIndex: number = 23;
    var line: string = "\tpublic class Searcher\n";
    var linesBefore: string[] = [];
    var linesAfter: string[] = [];
    var result: SearchResult = new SearchResult(pattern, file, linenum, matchStartIndex,
        matchEndIndex, line, linesBefore, linesAfter);
    var resultString: string = result.toString();
    var expectedOutput: string = file + ": " + linenum + ": [" + matchStartIndex + ":" +
        matchEndIndex + "]: " + line.trim();
    test.ok(resultString === expectedOutput, "single-line result matches expected");
    test.done();
};

exports.testBinaryFileResult = function(test) {
    var pattern: RegExp = /Search/;
    var file: string = cssearch_path + '/bin/Debug/CsSearch.exe';
    var linenum: number = 0;
    var matchStartIndex: number = 5;
    var matchEndIndex: number = 10;
    var line: string = "";
    var linesBefore: string[] = [];
    var linesAfter: string[] = [];
    var result: SearchResult = new SearchResult(pattern, file, linenum, matchStartIndex,
        matchEndIndex, line, linesBefore, linesAfter);
    var resultString: string = result.toString();
    var expectedOutput: string = file + " matches at [5:10]";
    test.ok(resultString === expectedOutput, "single-line result matches expected");
    test.done();
};

exports.testMultiLineResult = function(test) {
    var pattern: RegExp = /Search/;
    var file: string = cssearch_path + '/Searcher.cs';
    var linenum: number = 10;
    var matchStartIndex: number = 15;
    var matchEndIndex: number = 23;
    var line: string = "\tpublic class Searcher\n";
    var linesBefore: string[] = ["namespace CsSearch\n", "{\n"];
    var linesAfter: string[] = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"];

    var result: SearchResult = new SearchResult(pattern, file, linenum, matchStartIndex,
        matchEndIndex, line, linesBefore, linesAfter);
    var resultString: string = result.toString();
    var expectedOutput = "" +
        "================================================================================\n" +
        `${file}: ${linenum}: [${matchStartIndex}:${matchEndIndex}]\n` +
        "--------------------------------------------------------------------------------\n" +
        "   8 | namespace CsSearch\n" +
        "   9 | {\n" +
        "> 10 | \tpublic class Searcher\n" +
        "  11 | \t{\n" +
        "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
    test.ok(resultString === expectedOutput, "multi-line result matches expected");
    test.done();
};
