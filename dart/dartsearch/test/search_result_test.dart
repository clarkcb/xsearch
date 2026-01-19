import 'dart:io' show File;

import 'package:dartfind/dartfind.dart';
import 'package:dartsearch/dartsearch.dart';
import 'package:test/test.dart';

void main() {
  test('test single-line search result', () {
    var settings = SearchSettings();
    settings.colorize = false;
    var formatter = SearchResultFormatter(settings);
    var pattern = RegExp('Search');
    var path = './lib/src/searcher.dart';
    var file = File(path);
    var fileResult = FileResult(file, FileType.code, 0, null);
    var lineNum = 13;
    var matchStartIndex = 7;
    var matchEndIndex = 15;
    var line = 'class Searcher {\n';
    var result = SearchResult(pattern, fileResult, lineNum, matchStartIndex,
        matchEndIndex, line, [], []);
    var expectedOutput =
        '$path: $lineNum: [$matchStartIndex:$matchEndIndex]: ${line.trim()}';
    var output = formatter.format(result);
    expect(output, expectedOutput);
  });

  test('test search result with line longer than maxlinelength', () {
    var settings = SearchSettings();
    settings.colorize = false;
    settings.maxLineLength = 100;
    var formatter = SearchResultFormatter(settings);
    var pattern = RegExp('maxlen');
    var path = './maxlen.txt';
    var file = File(path);
    var fileResult = FileResult(file, FileType.text, 0, null);
    var lineNum = 1;
    var matchStartIndex = 53;
    var matchEndIndex = 59;
    var line =
        '0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789';
    var result = SearchResult(pattern, fileResult, lineNum, matchStartIndex,
        matchEndIndex, line, [], []);
    var expectedLine =
        '...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...';
    var expectedOutput =
        '$path: $lineNum: [$matchStartIndex:$matchEndIndex]: $expectedLine';
    var output = formatter.format(result);
    expect(output, expectedOutput);
  });

  test('test search result with colorized line longer than maxlinelength', () {
    var settings = SearchSettings();
    settings.colorize = true;
    settings.maxLineLength = 100;
    var formatter = SearchResultFormatter(settings);
    var pattern = RegExp('maxlen');
    var path = './maxlen.txt';
    var file = File(path);
    var fileResult = FileResult(file, FileType.text, 0, null);
    var lineNum = 1;
    var matchStartIndex = 53;
    var matchEndIndex = 59;
    var line =
        '0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789';
    var result = SearchResult(pattern, fileResult, lineNum, matchStartIndex,
        matchEndIndex, line, [], []);
    var expectedLine =
        '...89012345678901234567890123456789012345678901${ConsoleColor.green}maxlen${ConsoleColor.reset}89012345678901234567890123456789012345678901...';
    var expectedOutput =
        '$path: $lineNum: [$matchStartIndex:$matchEndIndex]: $expectedLine';
    var output = formatter.format(result);
    expect(output, expectedOutput);
  });

  test('test binary file search result', () {
    var settings = SearchSettings();
    var formatter = SearchResultFormatter(settings);
    var pattern = RegExp('Search');
    var path = './lib/src/searcher.exe';
    var file = File(path);
    var fileResult = FileResult(file, FileType.binary, 0, null);
    var lineNum = 0;
    var matchStartIndex = 0;
    var matchEndIndex = 0;
    var result = SearchResult(pattern, fileResult, lineNum, matchStartIndex,
        matchEndIndex, null, [], []);
    var expectedOutput = '$path matches at [$matchStartIndex:$matchEndIndex]';
    var output = formatter.format(result);
    expect(output, expectedOutput);
  });

  test('test multiline search result', () {
    var settings = SearchSettings();
    settings.colorize = false;
    settings.linesBefore = 2;
    settings.linesAfter = 2;
    var formatter = SearchResultFormatter(settings);
    var pattern = RegExp('Searcher');
    var path = '~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs';
    var file = File(path);
    var fileResult = FileResult(file, FileType.code, 0, null);
    var lineNum = 10;
    var matchStartIndex = 15;
    var matchEndIndex = 23;
    var line = '\tpublic class Searcher';
    var linesBefore = ['namespace CsSearch', '{'];
    var linesAfter = ['\t{', '\t\tprivate readonly FileTypes _fileTypes;'];
    var result = SearchResult(pattern, fileResult, lineNum, matchStartIndex,
        matchEndIndex, line, linesBefore, linesAfter);
    var expectedOutput =
        '================================================================================\n'
        '$path: $lineNum: [$matchStartIndex:$matchEndIndex]\n'
        '--------------------------------------------------------------------------------\n'
        '   8 | namespace CsSearch\n'
        '   9 | {\n'
        '> 10 | 	public class Searcher\n'
        '  11 | 	{\n'
        '  12 | 		private readonly FileTypes _fileTypes;\n';

    var output = formatter.format(result);
    expect(output, expectedOutput);
  });
}
