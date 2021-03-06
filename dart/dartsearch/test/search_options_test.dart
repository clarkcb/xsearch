import 'package:dartsearch/dartsearch.dart' show SearchOptions, SearchSettings;
import 'package:test/test.dart';

void main() {
  test('test get settings from minimal args', () async {
    var options = SearchOptions();
    var settings = await options.settingsFromArgs(['-s', 'Search', '.']);
    expect(settings.archivesOnly, false);
    expect(settings.colorize, true);
    expect(settings.debug, false);
    expect(settings.excludeHidden, true);
    expect(settings.firstMatch, false);
    expect(settings.linesAfter, 0);
    expect(settings.linesBefore, 0);
    expect(settings.listDirs, false);
    expect(settings.listFiles, false);
    expect(settings.listLines, false);
    expect(settings.maxLineLength, 150);
    expect(settings.multiLineSearch, false);
    expect(settings.printResults, true);
    expect(settings.printUsage, false);
    expect(settings.printVersion, false);
    expect(settings.searchArchives, false);
    expect(settings.recursive, true);
    expect(settings.uniqueLines, false);
    expect(settings.verbose, false);
    expect(settings.searchPatterns.length, 1);
    expect((settings.searchPatterns.first as RegExp).pattern, 'Search');
    expect(settings.startPath, '.');
  });

  test('test get settings from valid args', () async {
    var options = SearchOptions();
    var settings = await options.settingsFromArgs(['-x', 'dart,kt', '-s', 'Search', '.']);
    expect(settings.inExtensions.length, 2);
    expect(settings.inExtensions.contains('dart'), true);
    expect(settings.inExtensions.contains('kt'), true);
    expect(settings.searchPatterns.length, 1);
    expect((settings.searchPatterns.first as RegExp).pattern, 'Search');
    expect(settings.startPath, '.');
  });

  test('test get settings from json', () async {
    var json = '{'
    '"startpath": "~/src/xsearch/",'
    '"in-ext": ["js","ts"],'
    '"out-dirpattern": ["build", "node_module", "tests", "typings"],'
    r'"out-filepattern": ["gulpfile", "\\.min\\."],'
    '"searchpattern": "Searcher",'
    '"linesbefore": 2,'
    '"linesafter": 2,'
    '"debug": true,'
    '"allmatches": false,'
    '"includehidden": false'
    '}';
    var options = SearchOptions();
    var settings = SearchSettings();
    await options.settingsFromJson(json, settings);

    expect(settings.startPath, '~/src/xsearch/');

    expect(settings.inExtensions.length, 2);
    expect(settings.inExtensions.contains('js'), true);
    expect(settings.inExtensions.contains('ts'), true);

    expect(settings.outDirPatterns.length, 4);
    expect(settings.outDirPatterns.any((p) => (p as RegExp).pattern == 'node_module'), true);

    expect(settings.outFilePatterns.length, 2);
    expect(settings.outFilePatterns.any((p) => (p as RegExp).pattern == 'gulpfile'), true);

    expect(settings.searchPatterns.length, 1);
    expect((settings.searchPatterns.first as RegExp).pattern, 'Searcher');

    expect(settings.linesBefore, 2);
    expect(settings.linesAfter, 2);

    expect(settings.debug, true);
    expect(settings.verbose, true);
    expect(settings.firstMatch, true);
    expect(settings.excludeHidden, true);
  });
}
