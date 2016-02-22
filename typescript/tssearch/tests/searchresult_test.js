/// <reference path="../typings/node/node.d.ts"/>
/// <reference path="../src/common.ts"/>
/// <reference path="../src/config.ts"/>
/// <reference path="../src/searchresult.ts"/>
/*
 * searchresult_test.js
 *
 * Some nodeunit tests of searchresult.js
 */
var common = require('../build/common.js');
var config = require('../build/config.js');
var SearchResult = require('../build/searchresult.js').SearchResult;
var cssearch_path = config.XSEARCHPATH + '/csharp/CsSearch/CsSearch';
exports.testSingleLineResult = function (test) {
    var pattern = /Search/;
    var file = cssearch_path + '/Searcher.cs';
    var linenum = 10;
    var matchStartIndex = 15;
    var matchEndIndex = 23;
    var line = "\tpublic class Searcher\n";
    var linesBefore = [];
    var linesAfter = [];
    var result = new SearchResult(pattern, file, linenum, matchStartIndex, matchEndIndex, line, linesBefore, linesAfter);
    var resultString = result.toString();
    var expectedOutput = file + ": " + linenum + ": [" + matchStartIndex + ":" +
        matchEndIndex + "]: " + line.trim();
    test.ok(resultString === expectedOutput, "single-line result matches expected");
    test.done();
};
exports.testBinaryFileResult = function (test) {
    var pattern = /Search/;
    var file = cssearch_path + '/bin/Debug/CsSearch.exe';
    var linenum = 0;
    var matchStartIndex = 5;
    var matchEndIndex = 10;
    var line = "";
    var linesBefore = [];
    var linesAfter = [];
    var result = new SearchResult(pattern, file, linenum, matchStartIndex, matchEndIndex, line, linesBefore, linesAfter);
    var resultString = result.toString();
    var expectedOutput = file + " matches at [5:10]";
    test.ok(resultString === expectedOutput, "single-line result matches expected");
    test.done();
};
exports.testMultiLineResult = function (test) {
    var pattern = /Search/;
    var file = cssearch_path + '/Searcher.cs';
    var linenum = 10;
    var matchStartIndex = 15;
    var matchEndIndex = 23;
    var line = "\tpublic class Searcher\n";
    var linesBefore = ["namespace CsSearch\n", "{\n"];
    var linesAfter = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"];
    var result = new SearchResult(pattern, file, linenum, matchStartIndex, matchEndIndex, line, linesBefore, linesAfter);
    var resultString = result.toString();
    var expectedOutput = "" +
        "================================================================================\n" +
        (file + ": " + linenum + ": [" + matchStartIndex + ":" + matchEndIndex + "]\n") +
        "--------------------------------------------------------------------------------\n" +
        "   8 | namespace CsSearch\n" +
        "   9 | {\n" +
        "> 10 | \tpublic class Searcher\n" +
        "  11 | \t{\n" +
        "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
    test.ok(resultString === expectedOutput, "multi-line result matches expected");
    test.done();
};
