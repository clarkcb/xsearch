import 'dart:io';

import 'package:dartfind/dartfind.dart';
import 'package:dartsearch/dartsearch.dart';

void _handleError(err, bool colorize, SearchOptions options) {
  logMsg('');
  logError('$err\n', colorize);
  options.usage();
  exitCode = 1;
}

Future<void> search(SearchSettings settings, SearchOptions options) async {
  var results = <SearchResult>[];
  try {
    var searcher = Searcher(settings);
    results = await searcher.search();
    var formatter = SearchResultFormatter(settings);

    if (results.isNotEmpty) {
      if (settings.printResults) {
        searcher.printResults(results, formatter);
      }

      if (settings.printDirs) {
        searcher.printMatchingDirs(results, formatter);
      }

      if (settings.printFiles) {
        searcher.printMatchingFiles(results, formatter);
      }

      if (settings.printLines) {
        searcher.printMatchingLines(results, formatter);
      }

      if (settings.printMatches) {
        searcher.printMatches(results, formatter);
      }
    }
  } on FormatException catch (e) {
    logError(e.message);
  } on SearchException catch (e) {
    _handleError(e, settings.colorize, options);
  } on FindException catch (e) {
    _handleError(e, settings.colorize, options);
  } catch (e) {
    print(e);
    rethrow;
  }
}

Future<void> main(List<String> arguments) async {
  // initialize as success
  exitCode = 0;

  var options = SearchOptions();

  await options.settingsFromArgs(arguments).then((settings) {
    if (settings.debug) logMsg('settings: $settings');
    if (settings.printUsage) {
      logMsg('');
      options.usage();
    } else {
      search(settings, options);
    }
  }).catchError((e) {
    _handleError(e, true, options);
  });
}
