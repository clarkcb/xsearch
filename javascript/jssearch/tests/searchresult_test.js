/*
 * searchresult_test.js
 *
 * Some nodeunit tests of searchresult.js
 */

const common = require('../src/common.js');
const config = require('../src/config.js');
const SearchResult = require('../src/searchresult.js').SearchResult;

const cssearch_path = config.XSEARCHPATH + '/csharp/CsSearch/CsSearch';

exports.testSingleLineResult = function(test) {
    const pattern = 'Search';
    const file = cssearch_path + '/Searcher.cs';
    const linenum = 10;
    const matchStartIndex = 15;
    const matchEndIndex = 23;
    const line = "\tpublic class Searcher\n";
    const linesBefore = [];
    const linesAfter = [];
    const result = new SearchResult(pattern, file, linenum, matchStartIndex,
        matchEndIndex, line, linesBefore, linesAfter);
    const resultString = result.toString();
    const expectedOutput = file + ": " + linenum + ": [" + matchStartIndex + ":" +
        matchEndIndex + "]: " + line.trim();
    test.ok(resultString === expectedOutput, "single-line result matches expected");
    test.done();
};

exports.testBinaryFileResult = function(test) {
    const pattern = 'Search';
    const file = cssearch_path + '/bin/Debug/CsSearch.exe';
    const linenum = 0;
    const matchStartIndex = 5;
    const matchEndIndex = 10;
    const line = "";
    const linesBefore = [];
    const linesAfter = [];
    const result = new SearchResult(pattern, file, linenum, matchStartIndex,
        matchEndIndex, line, linesBefore, linesAfter);
    const resultString = result.toString();
    const expectedOutput = file + " matches at [5:10]";
    test.ok(resultString === expectedOutput, "single-line result matches expected");
    test.done();
};

exports.testMultiLineResult = function(test) {
    const pattern = 'Search';
    const file = cssearch_path + '/Searcher.cs';
    const linenum = 10;
    const matchStartIndex = 15;
    const matchEndIndex = 23;
    const line = "\tpublic class Searcher\n";
    const linesBefore = ["namespace CsSearch\n", "{\n"];
    const linesAfter = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"];

    const result = new SearchResult(pattern, file, linenum, matchStartIndex,
        matchEndIndex, line, linesBefore, linesAfter);
    const resultString = result.toString();
    const expectedOutput = "" +
        "================================================================================\n" +
        `${file}: ${linenum}: [${matchStartIndex}:${matchEndIndex}]\n` +
        "--------------------------------------------------------------------------------\n" +
        "   8 | namespace CsSearch\n" +
        "   9 | {\n" +
        "> 10 | \tpublic class Searcher\n" +
        "  11 | \t{\n" +
        "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
    common.log("\nexpectedOutput:\n\"" + expectedOutput + "\"");
    common.log("\nresultString:\n\"" + resultString + "\"");
    test.ok(resultString === expectedOutput, "multi-line result matches expected");
    test.done();
};
