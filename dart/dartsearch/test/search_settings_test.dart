import 'package:dartsearch/dartsearch.dart' show SearchSettings;
import 'package:test/test.dart';

void main() {
  test('test default settings', () {
    var settings = SearchSettings();
    expect(settings.archivesOnly, false);
    expect(settings.colorize, true);
    expect(settings.debug, false);
    expect(settings.firstMatch, false);
    expect(settings.followSymlinks, false);
    expect(settings.includeHidden, false);
    expect(settings.linesAfter, 0);
    expect(settings.linesBefore, 0);
    expect(settings.maxLineLength, 150);
    expect(settings.multiLineSearch, false);
    expect(settings.paths.length, 0);
    expect(settings.printDirs, false);
    expect(settings.printFiles, false);
    expect(settings.printLines, false);
    expect(settings.printResults, false);
    expect(settings.printUsage, false);
    expect(settings.printVersion, false);
    expect(settings.searchArchives, false);
    expect(settings.recursive, true);
    expect(settings.uniqueLines, false);
    expect(settings.verbose, false);
  });

  test('test add extensions', () {
    var settings = SearchSettings();
    settings.addExtensions('dart,kt', settings.inExtensions);
    expect(settings.inExtensions.length, 2);
    expect(settings.inExtensions.contains('dart'), true);
    expect(settings.inExtensions.contains('kt'), true);
  });

  test('test add extensionsList', () {
    var settings = SearchSettings();
    settings.addExtensionsList(['dart', 'kt'], settings.inExtensions);
    expect(settings.inExtensions.length, 2);
    expect(settings.inExtensions.contains('dart'), true);
    expect(settings.inExtensions.contains('kt'), true);
  });

  test('test add pattern', () {
    var settings = SearchSettings();
    settings.addPattern('Searcher', settings.searchPatterns);
    expect(settings.searchPatterns.length, 1);
    expect(settings.searchPatterns.first is RegExp, true);
    expect((settings.searchPatterns.first as RegExp).pattern, 'Searcher');
    expect((settings.searchPatterns.first as RegExp).isMultiLine, true);
  });

  test('test set archivesOnly', () {
    var settings = SearchSettings();
    expect(settings.archivesOnly, false);
    expect(settings.searchArchives, false);
    settings.archivesOnly = true;
    expect(settings.archivesOnly, true);
    expect(settings.includeArchives, true);
  });

  test('test set debug', () {
    var settings = SearchSettings();
    expect(settings.debug, false);
    expect(settings.verbose, false);
    settings.debug = true;
    expect(settings.debug, true);
    expect(settings.verbose, true);
  });
}
