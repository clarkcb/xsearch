import 'dart:convert' show json;
import 'dart:io';

import 'package:dartfind/dartfind.dart';
import 'package:dartsearch/src/config.dart' show searchOptionsPath;
import 'package:dartsearch/src/search_settings.dart';

class SearchOption implements Option {
  final String? _shortArg;
  final String _longArg;
  final String _desc;
  final ArgTokenType _argType;

  const SearchOption(this._shortArg, this._longArg, this._desc, this._argType);

  @override
  String? shortArg() => _shortArg;

  @override
  String longArg() => _longArg;

  @override
  String desc() => _desc;

  @override
  ArgTokenType argType() => _argType;

  String sortArg() {
    if (_shortArg == null) {
      return _longArg.toLowerCase();
    } else {
      return '${_shortArg!.toLowerCase()}@${_longArg.toLowerCase()}';
    }
  }

  String optString() {
    if (_shortArg == null) {
      return '--$_longArg';
    } else {
      return '-$_shortArg,--$_longArg';
    }
  }
}

class SearchOptions {
  List<SearchOption> searchOptions = [];
  var boolActionMap = {};
  var stringActionMap = {};
  var intActionMap = {};
  var longArgMap = {'path': 'path'};
  ArgTokenizer? argTokenizer;
  late Future ready;

  SearchOptions() {
    setActionMaps();
    ready = loadSearchOptionsFromJson().then((f) => setArgTokenizer());
  }

  Future<void> loadSearchOptionsFromJson() async {
    var contents = await File(searchOptionsPath).readAsString();
    Map soMap = json.decode(contents);
    if (soMap.containsKey('searchoptions')) {
      var soList = soMap['searchoptions'] as List;
      for (var so in soList) {
        var longArg = (so as Map)['long'];
        var desc = (so)['desc']!;
        String? shortArg;
        if ((so).containsKey('short')) {
          shortArg = (so)['short'];
        }
        var argType = ArgTokenType.unknownType;
        if (boolActionMap.containsKey(longArg)) {
          argType = ArgTokenType.boolType;
        } else if (stringActionMap.containsKey(longArg) ||
            longArg == 'settings-file') {
          argType = ArgTokenType.stringType;
        } else if (intActionMap.containsKey(longArg)) {
          argType = ArgTokenType.intType;
        } else {
          throw FindException('No action for option: $longArg');
        }
        searchOptions.add(SearchOption(shortArg, longArg, desc, argType));
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
      'noprintmatches': (bool b, SearchSettings ss) => ss.printMatches = !b,
      'noprintresults': (bool b, SearchSettings ss) => ss.printResults = !b,
      'norecursive': (bool b, SearchSettings ss) => ss.recursive = !b,
      'nosearcharchives': (bool b, SearchSettings ss) => ss.searchArchives = !b,
      'printdirs': (bool b, SearchSettings ss) => ss.printDirs = b,
      'printfiles': (bool b, SearchSettings ss) => ss.printFiles = b,
      'printlines': (bool b, SearchSettings ss) => ss.printLines = b,
      'printmatches': (bool b, SearchSettings ss) => ss.printMatches = b,
      'printresults': (bool b, SearchSettings ss) => ss.printResults = b,
      'printusage': (bool b, SearchSettings ss) => ss.printUsage = b,
      'recursive': (bool b, SearchSettings ss) => ss.recursive = b,
      'searcharchives': (bool b, SearchSettings ss) => ss.searchArchives = b,
      'sort-ascending': (bool b, SearchSettings ss) => ss.sortDescending = !b,
      'sort-caseinsensitive': (bool b, SearchSettings ss) =>
          ss.sortCaseInsensitive = b,
      'sort-casesensitive': (bool b, SearchSettings ss) =>
          ss.sortCaseInsensitive = !b,
      'sort-descending': (bool b, SearchSettings ss) => ss.sortDescending = b,
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

  void setArgTokenizer() {
    argTokenizer = ArgTokenizer(searchOptions);
  }

  Future<void> updateSettingsFromArgTokens(
      SearchSettings settings, List<ArgToken> argTokens) async {
    await ready.then((_) async {
      for (var argToken in argTokens) {
        if (argToken.tokenType == ArgTokenType.boolType) {
          if (boolActionMap.containsKey(argToken.name)) {
            boolActionMap[argToken.name](argToken.value, settings);
          } else {
            throw FindException('Invalid option: ${argToken.name}');
          }
        } else if (argToken.tokenType == ArgTokenType.stringType) {
          if (stringActionMap.containsKey(argToken.name)) {
            stringActionMap[argToken.name](argToken.value, settings);
          } else if (argToken.name == 'settings-file') {
            await updateSettingsFromFile(settings, argToken.value);
          } else {
            throw FindException('Invalid option: ${argToken.name}');
          }
        } else if (argToken.tokenType == ArgTokenType.intType) {
          if (intActionMap.containsKey(argToken.name)) {
            intActionMap[argToken.name](argToken.value, settings);
          } else {
            throw FindException('Invalid option: ${argToken.name}');
          }
        } else {
          throw FindException('Invalid option: ${argToken.name}');
        }
      }
    });
  }

  Future<void> updateSettingsFromJson(
      SearchSettings settings, String jsonString) async {
    await ready.then((_) async {
      List<ArgToken> argTokens = argTokenizer!.tokenizeJson(jsonString);
      await updateSettingsFromArgTokens(settings, argTokens);
    });
  }

  Future<void> updateSettingsFromFile(
      SearchSettings settings, String filePath) async {
    List<ArgToken> argTokens = await argTokenizer!.tokenizeFile(filePath);
    await updateSettingsFromArgTokens(settings, argTokens);
  }

  Future<void> updateSettingsFromArgs(
      SearchSettings settings, List<String> args) async {
    await ready.then((_) async {
      var argTokens = argTokenizer!.tokenizeArgs(args);
      await updateSettingsFromArgTokens(settings, argTokens);
      return settings;
    });
  }

  Future<SearchSettings> settingsFromArgs(List<String> args) async {
    var settings = SearchSettings();
    settings.printResults = true; // default to printing results
    await updateSettingsFromArgs(settings, args);
    return settings;
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
        s += '${searchOptions[i].desc()}\n';
      }
      return s;
    });
  }

  void usage() async {
    logMsg(await getUsageString());
  }
}
