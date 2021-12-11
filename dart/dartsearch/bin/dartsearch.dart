import 'dart:io';

import 'package:dartsearch/dartsearch.dart';

void _handleError(err, SearchOptions options) {
  log('');
  logError(err.toString() + '\n');
  options.usage();
  exitCode = 1;
}

int sortResults(SearchResult r1, SearchResult r2) {
  if (r1.file.file.parent.path == r2.file.file.parent.path) {
    if (r1.file.file.path == r2.file.file.path) {
      if (r1.lineNum == r2.lineNum) {
        return r1.matchStartIndex.compareTo(r2.matchStartIndex);
      } else {
        return r1.lineNum.compareTo(r2.lineNum);
      }
    } else {
      return r1.file.file.path.compareTo(r2.file.file.path);
    }
  } else {
    return r1.file.file.parent.path.compareTo(r2.file.file.parent.path);
  }
}

void printResults(List<SearchResult> results, SearchSettings settings) {
  log('\nSearch results (${results.length}):');
  var formatter = SearchResultFormatter(settings);
  results.sort(sortResults);
  for (var r in results) {
    log(formatter.format(r));
  }
}

List<String> getMatchingDirs(List<SearchResult> results) {
  var dirs = results.map((r) => r.file.file.parent.path).toSet().toList();
  dirs.sort();
  return dirs;
}

void printMatchingDirs(List<SearchResult> results, SearchSettings settings) {
  var dirs = getMatchingDirs(results);
  if (dirs.isNotEmpty) {
    log('\nDirectories with matches (${dirs.length}):');
    dirs.forEach(log);
  }
}

int sortFiles(File f1, File f2) {
  if (f1.parent.path == f2.parent.path) {
    return f1.path.compareTo(f2.path);
  } else {
    return f1.parent.path.compareTo(f2.parent.path);
  }
}

List<String> getMatchingFiles(
    List<SearchResult> results, SearchSettings settings) {
  var files = results.map((r) => r.file.file).toSet().toList();
  files.sort(sortFiles);
  return files.map((f) => FileUtil.contractPath(f.path)).toList();
}

void printMatchingFiles(List<SearchResult> results, SearchSettings settings) {
  var files = getMatchingFiles(results, settings);
  if (files.isNotEmpty) {
    log('\nFiles with matches (${files.length}):');
    files.forEach(log);
  }
}

List<String> getMatchingLines(
    List<SearchResult> results, SearchSettings settings) {
  var lines = results.map((r) => r.line.trim()).toList();
  if (settings.uniqueLines) {
    lines = lines.toSet().toList();
  }
  lines.sort((l1, l2) => l1.toLowerCase().compareTo(l2.toLowerCase()));
  return lines;
}

void printMatchingLines(List<SearchResult> results, SearchSettings settings) {
  var lines = getMatchingLines(results, settings);
  if (lines.isNotEmpty) {
    String msg;
    if (settings.uniqueLines) {
      msg = '\nUnique lines with matches (${lines.length}):';
    } else {
      msg = '\nLines with matches (${lines.length}):';
    }
    log(msg);
    for (var l in lines) {
      log(l);
    }
  }
}

Future<void> search(SearchSettings settings, SearchOptions options) async {
  var results = <SearchResult>[];
  try {
    var searcher = Searcher(settings);
    results = await searcher.search();
  } on FormatException catch (e) {
    logError(e.message);
  } on SearchException catch (e) {
    _handleError(e, options);
  } catch (e) {
    print(e);
    rethrow;
  }

  if (results.isNotEmpty) {
    if (settings.printResults) {
      printResults(results, settings);
    }

    if (settings.listDirs) {
      printMatchingDirs(results, settings);
    }

    if (settings.listFiles) {
      printMatchingFiles(results, settings);
    }

    if (settings.listLines) {
      printMatchingLines(results, settings);
    }
  }
}

Future<void> main(List<String> arguments) async {
  // initialize as success
  exitCode = 0;

  var options = SearchOptions();

  await options.settingsFromArgs(arguments).then((settings) {
    if (settings.debug) log('settings: $settings');
    if (settings.printUsage) {
      log('');
      options.usage();
    } else {
      search(settings, options);
    }
  }).catchError((e) {
    _handleError(e, options);
  });
}
