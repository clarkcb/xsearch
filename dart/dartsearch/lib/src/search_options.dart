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
  var searchOptions = [];
  var stringArgMap = {};
  var boolArgMap = {};
  var longArgMap = {};
  late Future ready;

  SearchOptions() {
    ready = loadSearchOptionsFromJson().then((f) => setMaps());
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

  void setMaps() {
    stringArgMap = {
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
      'linesafter': (String s, SearchSettings ss) =>
          ss.linesAfter = int.parse(s),
      'linesaftertopattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.linesAfterToPatterns),
      'linesafteruntilpattern': (String s, SearchSettings ss) =>
          ss.addPattern(s, ss.linesAfterUntilPatterns),
      'linesbefore': (String s, SearchSettings ss) =>
          ss.linesBefore = int.parse(s),
      'maxlastmod': (String s, FindSettings ss) =>
          ss.maxLastMod = DateTime.parse(s),
      'maxlinelength': (String s, SearchSettings ss) =>
          ss.maxLineLength = int.parse(s),
      'maxsize': (String s, FindSettings ss) => ss.maxSize = int.parse(s),
      'minlastmod': (String s, FindSettings ss) =>
          ss.minLastMod = DateTime.parse(s),
      'minsize': (String s, FindSettings ss) => ss.minSize = int.parse(s),
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
      'sort-by': (String s, FindSettings ss) => ss.sortBy = nameToSortBy(s),
    };

    boolArgMap = {
      'archivesonly': (bool b, SearchSettings ss) => ss.archivesOnly = b,
      'allmatches': (bool b, SearchSettings ss) => ss.firstMatch = !b,
      'colorize': (bool b, SearchSettings ss) => ss.colorize = b,
      'debug': (bool b, SearchSettings ss) => ss.debug = b,
      'excludehidden': (bool b, SearchSettings ss) => ss.excludeHidden = b,
      'firstmatch': (bool b, SearchSettings ss) => ss.firstMatch = b,
      'help': (bool b, SearchSettings ss) => ss.printUsage = b,
      'includehidden': (bool b, SearchSettings ss) => ss.excludeHidden = !b,
      'listdirs': (bool b, SearchSettings ss) => ss.listDirs = b,
      'listfiles': (bool b, SearchSettings ss) => ss.listFiles = b,
      'listlines': (bool b, SearchSettings ss) => ss.listLines = b,
      'multilinesearch': (bool b, SearchSettings ss) => ss.multiLineSearch = b,
      'noprintmatches': (bool b, SearchSettings ss) => ss.printResults = !b,
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
  }

  Future<void> settingsFromJson(
      String jsonString, SearchSettings settings) async {
    await ready.then((_) {
      Map jsonMap = json.decode(jsonString);
      jsonMap.forEach((key, value) {
        if (stringArgMap.containsKey(key)) {
          if (value is String) {
            stringArgMap[key](value, settings);
          } else if (value is num) {
            stringArgMap[key]('$value', settings);
          } else {
            value.forEach((elem) {
              stringArgMap[key](elem, settings);
            });
          }
        } else if (boolArgMap.containsKey(key)) {
          if (value is bool) {
            boolArgMap[key](value, settings);
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
            if (stringArgMap.containsKey(longArg)) {
              if (it.moveNext()) {
                var s = it.current;
                stringArgMap[longArg](s, settings);
              } else {
                throw SearchException('Missing value for option $arg');
              }
            } else if (boolArgMap.containsKey(longArg)) {
              boolArgMap[longArg](true, settings);
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
