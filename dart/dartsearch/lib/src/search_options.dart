import 'dart:convert' show json;
import 'dart:io' show File;

import 'package:dartfind/dartfind.dart';
import 'package:dartsearch/src/config.dart' show searchOptionsPath;
import 'package:dartsearch/src/search_exception.dart';
import 'package:dartsearch/src/search_settings.dart';

class SearchOption {
  final String? shortArg;
  final String longArg;
  final String desc;

  const SearchOption(this.shortArg, this.longArg, this.desc);

  String sortArg() {
    if (shortArg == null) {
      return longArg.toLowerCase();
    } else {
      return '${shortArg!.toLowerCase()}@${longArg.toLowerCase()}';
    }
  }

  String optString() {
    if (shortArg == null) {
      return '--$longArg';
    } else {
      return '-$shortArg,--$longArg';
    }
  }
}

class SearchOptions {
  List<SearchOption> searchOptions = [];
  var boolActionMap = {};
  var stringActionMap = {};
  var intActionMap = {};
  var longArgMap = {};
  late Future ready;

  SearchOptions() {
    ready = loadSearchOptionsFromJson().then((f) => setActionMaps());
  }

  Future<void> loadSearchOptionsFromJson() async {
    var contents = await File(searchOptionsPath).readAsString();
    Map soMap = json.decode(contents);
    if (soMap.containsKey('searchoptions')) {
      var soList = soMap['searchoptions'] as List;
      for (var so in soList) {
        var longArg = (so as Map)['long'];
        longArgMap[longArg] = longArg;
        var desc = (so)['desc']!;
        String? shortArg;
        if ((so).containsKey('short')) {
          shortArg = (so)['short'];
          longArgMap[shortArg] = longArg;
        }
        searchOptions.add(SearchOption(shortArg, longArg, desc));
      }
    }
  }

  void setActionMaps() {
    boolActionMap = {
      'archivesonly': (bool b, SearchSettings ss) => ss.archivesOnly = b,
      'allmatches': (bool b, SearchSettings ss) => ss.firstMatch = !b,
      'colorize': (bool b, SearchSettings ss) => ss.colorize = b,
      'debug': (bool b, SearchSettings ss) => ss.debug = b,
      'excludehidden': (bool b, SearchSettings ss) => ss.includeHidden = !b,
      'firstmatch': (bool b, SearchSettings ss) => ss.firstMatch = b,
      'followsymlinks': (bool b, SearchSettings ss) => ss.followSymlinks = b,
      'help': (bool b, SearchSettings ss) => ss.printUsage = b,
      'includehidden': (bool b, SearchSettings ss) => ss.includeHidden = b,
      'multilinesearch': (bool b, SearchSettings ss) => ss.multiLineSearch = b,
      'nocolorize': (bool b, SearchSettings ss) => ss.colorize = !b,
      'nofollowsymlinks': (bool b, SearchSettings ss) => ss.followSymlinks = !b,
      'noprintdirs': (bool b, SearchSettings ss) => ss.printDirs = !b,
      'noprintfiles': (bool b, SearchSettings ss) => ss.printFiles = !b,
      'noprintlines': (bool b, SearchSettings ss) => ss.printLines = !b,
      'noprintmatches': (bool b, SearchSettings ss) => ss.printResults = !b,
      'printdirs': (bool b, SearchSettings ss) => ss.printDirs = b,
      'printfiles': (bool b, SearchSettings ss) => ss.printFiles = b,
      'printlines': (bool b, SearchSettings ss) => ss.printLines = b,
      'printmatches': (bool b, SearchSettings ss) => ss.printResults = b,
      'printusage': (bool b, SearchSettings ss) => ss.printUsage = b,
      'recursive': (bool b, SearchSettings ss) => ss.recursive = b,
      'searcharchives': (bool b, SearchSettings ss) => ss.searchArchives = b,
      'sort-ascending': (bool b, FindSettings ss) => ss.sortDescending = !b,
      'sort-caseinsensitive': (bool b, FindSettings ss) =>
          ss.sortCaseInsensitive = b,
      'sort-casesensitive': (bool b, FindSettings ss) =>
          ss.sortCaseInsensitive = !b,
      'sort-descending': (bool b, FindSettings ss) => ss.sortDescending = b,
      'uniquelines': (bool b, SearchSettings ss) => ss.uniqueLines = b,
      'verbose': (bool b, SearchSettings ss) => ss.verbose = b,
      'version': (bool b, SearchSettings ss) => ss.printVersion = b,
    };

    stringActionMap = {
      'encoding': (String s, SearchSettings ss) => ss.textFileEncoding = s,
      'in-archiveext': (String s, SearchSettings ss) =>
          ss.addExtensions(s, ss.inArchiveExtensions),
      'in-archivefilepattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.inArchiveFilePatterns),
      'in-dirpattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.inDirPatterns),
      'in-ext': (String s, SearchSettings ss) =>
          ss.addExtensions(s, ss.inExtensions),
      'in-filepattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.inFilePatterns),
      'in-filetype': (String s, SearchSettings ss) =>
          ss.inFileTypes.add(FileTypes.fromName(s)),
      'in-linesafterpattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.inLinesAfterPatterns),
      'in-linesbeforepattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.inLinesBeforePatterns),
      'linesaftertopattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.linesAfterToPatterns),
      'linesafteruntilpattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.linesAfterUntilPatterns),
      'maxlastmod': (String s, SearchSettings ss) =>
          ss.maxLastMod = DateTime.parse(s),
      'minlastmod': (String s, SearchSettings ss) =>
          ss.minLastMod = DateTime.parse(s),
      'out-archiveext': (String s, SearchSettings ss) =>
          ss.addExtensions(s, ss.outArchiveExtensions),
      'out-archivefilepattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.outArchiveFilePatterns),
      'out-dirpattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.outDirPatterns),
      'out-ext': (String s, SearchSettings ss) =>
          ss.addExtensions(s, ss.outExtensions),
      'out-filepattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.outFilePatterns),
      'out-filetype': (String s, SearchSettings ss) =>
          ss.outFileTypes.add(FileTypes.fromName(s)),
      'out-linesafterpattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.outLinesAfterPatterns),
      'out-linesbeforepattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.outLinesBeforePatterns),
      'path': (String s, SearchSettings ss) => ss.paths.add(s),
      'searchpattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.searchPatterns),
      'sort-by': (String s, SearchSettings ss) => ss.sortBy = nameToSortBy(s),
    };

    intActionMap = {
      'linesafter': (int i, SearchSettings ss) => ss.linesAfter = i,
      'linesbefore': (int i, SearchSettings ss) => ss.linesBefore = i,
      'maxdepth': (int i, SearchSettings ss) => ss.maxDepth = i,
      'maxlinelength': (int i, SearchSettings ss) => ss.maxLineLength = i,
      'maxsize': (int i, SearchSettings ss) => ss.maxSize = i,
      'mindepth': (int i, SearchSettings ss) => ss.minDepth = i,
      'minsize': (int i, SearchSettings ss) => ss.minSize = i,
    };
  }

  Future<void> settingsFromJson(
      String jsonString, SearchSettings settings) async {
    await ready.then((_) {
      Map jsonMap = json.decode(jsonString);
      jsonMap.forEach((key, value) {
        if (boolActionMap.containsKey(key)) {
          if (value is bool) {
            boolActionMap[key](value, settings);
          } else {
            logError('Invalid value for option $key');
          }
        } else if (stringActionMap.containsKey(key)) {
          if (value is String) {
            stringActionMap[key](value, settings);
          } else if (value is num) {
            stringActionMap[key]('$value', settings);
          } else {
            value.forEach((elem) {
              stringActionMap[key](elem, settings);
            });
          }
        } else if (intActionMap.containsKey(key)) {
          if (value is int) {
            intActionMap[key](value, settings);
          } else {
            logError('Invalid value for option $key');
          }
        } else {
          logError('Invalid option: $key');
        }
      });
    });
  }

  Future<void> settingsFromFile(
      String filePath, SearchSettings settings) async {
    var contents = await File(filePath).readAsString();
    await settingsFromJson(contents, settings);
  }

  Future<SearchSettings> settingsFromArgs(List<String> args) async {
    return await ready.then((_) async {
      var settings = SearchSettings();
      // default printResults to true since running as cli
      settings.printResults = true;
      var it = args.iterator;
      while (it.moveNext()) {
        var arg = it.current;
        if (arg.startsWith('-')) {
          while (arg.startsWith('-')) {
            arg = arg.substring(1);
          }
          if (longArgMap.containsKey(arg)) {
            String longArg = longArgMap[arg];
            if (boolActionMap.containsKey(longArg)) {
              boolActionMap[longArg](true, settings);
            } else if (stringActionMap.containsKey(longArg) ||
                intActionMap.containsKey(longArg)) {
              if (it.moveNext()) {
                var s = it.current;
                if (stringActionMap.containsKey(longArg)) {
                  stringActionMap[longArg](s, settings);
                } else {
                  intActionMap[longArg](int.parse(s), settings);
                }
              } else {
                throw SearchException('Missing value for option $arg');
              }
            } else if (longArg == 'settings-file') {
              if (it.moveNext()) {
                var s = it.current;
                await settingsFromFile(s, settings);
              } else {
                throw SearchException('Missing value for option $arg');
              }
            } else {
              throw SearchException('Invalid option: $arg');
            }
          } else {
            throw SearchException('Invalid option: $arg');
          }
        } else {
          settings.paths.add(arg);
        }
      }
      return settings;
    });
  }

  void usage() async {
    logMsg(await getUsageString());
  }

  Future<String> getUsageString() async {
    return await ready.then((_) {
      var s = 'Usage:\n'
          ' dartsearch [options] -s <searchpattern> <path> [<path> ...]\n\n'
          'Options:\n';
      searchOptions.sort((o1, o2) => o1.sortArg().compareTo(o2.sortArg()));
      var optStrings = searchOptions.map((so) => so.optString()).toList();
      var longest = optStrings.reduce((value, optString) =>
          (optString.length > value.length) ? optString : value);
      for (var i = 0; i < searchOptions.length; i++) {
        s += ' ${optStrings[i].padRight(longest.length + 2, ' ')}';
        s += '${searchOptions[i].desc}\n';
      }
      return s;
    });
  }
}
