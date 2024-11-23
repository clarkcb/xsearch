import 'dart:io';
import 'dart:convert';

import 'package:dartfind/dartfind.dart';
import 'package:dartsearch/dartsearch.dart';
import 'package:test/test.dart';

void main() {
  SearchSettings getSettings() {
    var settings = SearchSettings();
    settings.paths.add('.');
    settings.addPattern('Searcher', settings.searchPatterns);
    return settings;
  }

  /***************************************************************************
   * searchLineStream tests
   **************************************************************************/
  test('test searchLineStream testFile2.txt', () async {
    var settings = getSettings();
    var searcher = Searcher(settings);
    var testFilePath = '$sharedPath/testFiles/testFile2.txt';
    var inputStream = File(testFilePath).openRead();
    var results = [];
    try {
      var lineStream = utf8.decoder.bind(inputStream).transform(LineSplitter());
      results = await searcher.searchLineStream(lineStream);
    } catch (e) {
      logError(e.toString());
    }

    expect(results.length, 2);

    var firstResult = results[0];
    expect(firstResult.lineNum, 30);
    expect(firstResult.matchStartIndex, 3);
    expect(firstResult.matchEndIndex, 11);

    var secondResult = results[1];
    expect(secondResult.lineNum, 36);
    expect(secondResult.matchStartIndex, 24);
    expect(secondResult.matchEndIndex, 32);
  });

  /***************************************************************************
   * searchMultiLineString tests
   **************************************************************************/
  test('test searchMultiLineString testFile2.txt', () async {
    var settings = getSettings();
    var searcher = Searcher(settings);
    var testFilePath = '$sharedPath/testFiles/testFile2.txt';

    var contents = await File(testFilePath).readAsString();
    var results = searcher.searchMultilineString(contents);

    expect(results.length, 2);

    var firstResult = results[0];
    expect(firstResult.lineNum, 30);
    expect(firstResult.matchStartIndex, 3);
    expect(firstResult.matchEndIndex, 11);

    var secondResult = results[1];
    expect(secondResult.lineNum, 36);
    expect(secondResult.matchStartIndex, 24);
    expect(secondResult.matchEndIndex, 32);
  });
}
