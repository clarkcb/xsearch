import 'dart:io';
import 'dart:convert';

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
   * isMatchingDir tests
   **************************************************************************/
  group('isMatchingDir tests', () {
    test('test isMatchingDir single dot', () {
      var settings = getSettings();
      var searcher = Searcher(settings);
      expect(searcher.isMatchingDir(Directory('.')), true);
    });

    test('test isMatchingDir double dot', () {
      var settings = getSettings();
      var searcher = Searcher(settings);
      expect(searcher.isMatchingDir(Directory('..')), true);
    });

    test('test isMatchingDir hidden dir', () {
      var settings = getSettings();
      var searcher = Searcher(settings);
      expect(searcher.isMatchingDir(Directory('.git')), false);
    });

    test('test isMatchingDir hidden dir includeHidden', () {
      var settings = getSettings();
      settings.excludeHidden = false;
      var searcher = Searcher(settings);
      expect(searcher.isMatchingDir(Directory('.git')), true);
    });

    test('test isMatchingDir no patterns', () {
      var settings = getSettings();
      settings.excludeHidden = false;
      var searcher = Searcher(settings);
      expect(searcher.isMatchingDir(Directory('/Users')), true);
    });

    test('test isMatchingDir dir matches inDirPatterns', () {
      var settings = getSettings();
      settings.addPattern('Search', settings.inDirPatterns);
      var searcher = Searcher(settings);
      expect(searcher.isMatchingDir(Directory('./CsSearch')), true);
    });

    test('test isMatchingDir dir does not match inDirPatterns', () {
      var settings = getSettings();
      settings.addPattern('SearchFiles', settings.inDirPatterns);
      var searcher = Searcher(settings);
      expect(searcher.isMatchingDir(Directory('./CsSearch')), false);
    });

    test('test isMatchingDir dir matches outDirPatterns', () {
      var settings = getSettings();
      settings.addPattern('Search', settings.outDirPatterns);
      var searcher = Searcher(settings);
      expect(searcher.isMatchingDir(Directory('./CsSearch')), false);
    });

    test('test isMatchingDir dir does not match outDirPatterns', () {
      var settings = getSettings();
      settings.addPattern('SearchFiles', settings.outDirPatterns);
      var searcher = Searcher(settings);
      expect(searcher.isMatchingDir(Directory('./CsSearch')), true);
    });
  });

  /***************************************************************************
   * isSearchFile tests
   **************************************************************************/
  group('isSearchFile tests', () {
    test('test isSearchFile no extensions no patterns', () {
      var settings = getSettings();
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('./FileUtil.cs'), FileType.code);
      expect(searcher.isSearchFile(searchFile), true);
    });

    test('test isSearchFile file extension matches inExtensions', () {
      var settings = getSettings();
      settings.addExtensions('cs', settings.inExtensions);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('./FileUtil.cs'), FileType.code);
      expect(searcher.isSearchFile(searchFile), true);
    });

    test('test isSearchFile file extension does not match inExtensions', () {
      var settings = getSettings();
      settings.addExtensions('java', settings.inExtensions);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('./FileUtil.cs'), FileType.code);
      expect(searcher.isSearchFile(searchFile), false);
    });

    test('test isSearchFile file extension matches outExtensions', () {
      var settings = getSettings();
      settings.addExtensions('cs', settings.outExtensions);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('./FileUtil.cs'), FileType.code);
      expect(searcher.isSearchFile(searchFile), false);
    });

    test('test isSearchFile file extension does not match outExtensions', () {
      var settings = getSettings();
      settings.addExtensions('java', settings.outExtensions);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('./FileUtil.cs'), FileType.code);
      expect(searcher.isSearchFile(searchFile), true);
    });

    test('test isSearchFile file name matches inFilePatterns', () {
      var settings = getSettings();
      settings.addPattern('Search', settings.inFilePatterns);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('./Searcher.cs'), FileType.code);
      expect(searcher.isSearchFile(searchFile), true);
    });

    test('test isSearchFile file name does not match inFilePatterns', () {
      var settings = getSettings();
      settings.addPattern('Search', settings.inFilePatterns);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('./FileUtil.cs'), FileType.code);
      expect(searcher.isSearchFile(searchFile), false);
    });

    test('test isSearchFile file name matches outFilePatterns', () {
      var settings = getSettings();
      settings.addPattern('Search', settings.outFilePatterns);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('./Searcher.cs'), FileType.code);
      expect(searcher.isSearchFile(searchFile), false);
    });

    test('test isSearchFile file name does not match outFilePatterns', () {
      var settings = getSettings();
      settings.addPattern('Search', settings.outFilePatterns);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('./FileUtil.cs'), FileType.code);
      expect(searcher.isSearchFile(searchFile), true);
    });
  });

  /***************************************************************************
   * isArchiveSearchFile tests
   **************************************************************************/
  group('isArchiveSearchFile tests', () {
    test('test isArchiveSearchFile no extensions no patterns', () {
      var settings = getSettings();
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('archive.zip'), FileType.archive);
      expect(searcher.isArchiveSearchFile(searchFile), true);
    });

    test('test isArchiveSearchFile file extension matches inArchiveExtensions',
        () {
      var settings = getSettings();
      settings.addExtensions('zip', settings.inArchiveExtensions);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('archive.zip'), FileType.archive);
      expect(searcher.isArchiveSearchFile(searchFile), true);
    });

    test(
        'test isArchiveSearchFile file extension does not match inArchiveExtensions',
        () {
      var settings = getSettings();
      settings.addExtensions('gz', settings.inArchiveExtensions);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('archive.zip'), FileType.archive);
      expect(searcher.isArchiveSearchFile(searchFile), false);
    });

    test('test isArchiveSearchFile file extension matches outArchiveExtensions',
        () {
      var settings = getSettings();
      settings.addExtensions('zip', settings.outArchiveExtensions);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('archive.zip'), FileType.archive);
      expect(searcher.isArchiveSearchFile(searchFile), false);
    });

    test(
        'test isArchiveSearchFile file extension does not match outArchiveExtensions',
        () {
      var settings = getSettings();
      settings.addExtensions('gz', settings.outArchiveExtensions);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('archive.zip'), FileType.archive);
      expect(searcher.isArchiveSearchFile(searchFile), true);
    });

    test('test isArchiveSearchFile file name matches inArchiveFilePatterns',
        () {
      var settings = getSettings();
      settings.addPattern('arch', settings.inArchiveFilePatterns);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('archive.zip'), FileType.archive);
      expect(searcher.isArchiveSearchFile(searchFile), true);
    });

    test(
        'test isArchiveSearchFile file name does not match inArchiveFilePatterns',
        () {
      var settings = getSettings();
      settings.addPattern('archives', settings.inArchiveFilePatterns);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('archive.zip'), FileType.archive);
      expect(searcher.isArchiveSearchFile(searchFile), false);
    });

    test('test isArchiveSearchFile file name matches outArchiveFilePatterns',
        () {
      var settings = getSettings();
      settings.addPattern('arch', settings.outArchiveFilePatterns);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('archive.zip'), FileType.archive);
      expect(searcher.isArchiveSearchFile(searchFile), false);
    });

    test(
        'test isArchiveSearchFile file name does not match outArchiveFilePatterns',
        () {
      var settings = getSettings();
      settings.addPattern('archives', settings.outArchiveFilePatterns);
      var searcher = Searcher(settings);
      var searchFile = SearchFile(File('archive.zip'), FileType.archive);
      expect(searcher.isArchiveSearchFile(searchFile), true);
    });
  });

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
      log(e.toString());
    }

    expect(results.length, 2);

    var firstResult = results[0];
    expect(firstResult.lineNum, 29);
    expect(firstResult.matchStartIndex, 3);
    expect(firstResult.matchEndIndex, 11);

    var secondResult = results[1];
    expect(secondResult.lineNum, 35);
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
    expect(firstResult.lineNum, 29);
    expect(firstResult.matchStartIndex, 3);
    expect(firstResult.matchEndIndex, 11);

    var secondResult = results[1];
    expect(secondResult.lineNum, 35);
    expect(secondResult.matchStartIndex, 24);
    expect(secondResult.matchEndIndex, 32);
  });
}
