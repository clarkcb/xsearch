/*
 * searchresult_test.js
 *
 * Some nodeunit tests of searchresult.js
 */

var common = require('../nodesearch/common.js');
var SearchResult = require('../nodesearch/searchresult.js').SearchResult;

exports.testSingleLineResult = function(test) {
    var pattern = 'Search';
    var file = '~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs';
    var linenum = 10;
    var matchStartIndex = 15;
    var matchEndIndex = 23;
    var line = "\tpublic class Searcher\n";
    var linesBefore = [];
    var linesAfter = [];
    var result = new SearchResult(pattern, file, linenum, matchStartIndex,
        matchEndIndex, line, linesBefore, linesAfter);
    var resultString = result.toString();
    var expectedOutput = file + ": " + linenum + ": [" + matchStartIndex + ":" +
        matchEndIndex + "]: " + line.trim();
    test.ok(resultString === expectedOutput, "single-line result matches expected");
    test.done();
}

exports.testBinaryFileResult = function(test) {
    var pattern = 'Search';
    var file = '~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.exe';
    var linenum = 0;
    var matchStartIndex = 0;
    var matchEndIndex = 0;
    var line = "";
    var linesBefore = [];
    var linesAfter = [];
    var result = new SearchResult(pattern, file, linenum, matchStartIndex,
        matchEndIndex, line, linesBefore, linesAfter);
    var resultString = result.toString();
    var expectedOutput = file + " matches";
    test.ok(resultString === expectedOutput, "single-line result matches expected");
    test.done();
}

exports.testMultiLineResult = function(test) {
    var pattern = 'Search';
    var file = '~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs';
    var linenum = 10;
    var matchStartIndex = 15;
    var matchEndIndex = 23;
    var line = "\tpublic class Searcher\n";
    var linesBefore = ["namespace CsSearch\n", "{\n"];
    var linesAfter = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"];

    var result = new SearchResult(pattern, file, linenum, matchStartIndex,
        matchEndIndex, line, linesBefore, linesAfter);
    var resultString = result.toString();
    var outputTemplate = "" +
        "================================================================================\n" +
        "{0}: {1}: [{2}:{3}]\n" +
        "--------------------------------------------------------------------------------\n" +
        "   8 | namespace CsSearch\n" +
        "   9 | {\n" +
        "> 10 | \tpublic class Searcher\n" +
        "  11 | \t{\n" +
        "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
    var expectedOutput = outputTemplate.format(file, linenum,
        matchStartIndex, matchEndIndex);
    test.ok(resultString === expectedOutput, "multi-line result matches expected");
    test.done();
}
