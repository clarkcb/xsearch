import 'dart:async';
import 'dart:io';
import 'dart:convert';
import 'dart:math';

import 'package:dartfind/dartfind.dart';

import 'package:dartsearch/src/search_exception.dart';
import 'package:dartsearch/src/search_result.dart';
import 'package:dartsearch/src/search_settings.dart';
// import 'package:path/path.dart' as path;

class Searcher {
  final SearchSettings settings;
  late final FileTypes _fileTypes;
  late final Finder _finder;
  // whether to allow invalid/malformed characters in encoding/decoding
  // setting to false means search will end once an attempt is made to read
  // a file in an incompatible encoding
  final bool allowInvalid = true;
  late final Encoding _binaryEncoding;
  Encoding? _textEncoding = systemEncoding;

  Searcher(this.settings) {
    _fileTypes = FileTypes();
    try {
      _finder = Finder(settings);
    } on FindException catch (e) {
      throw SearchException(e.message);
    } catch (e) {
      throw SearchException(
          'Unknown error occurred trying to instantiate finder');
    }
    _validateSettings();
    _binaryEncoding = Latin1Codec(allowInvalid: allowInvalid);
  }

  void _validateSettings() {
    if (settings.searchPatterns.isEmpty) {
      throw SearchException('No search patterns defined');
    }
    if (settings.linesAfter < 0) {
      throw SearchException('Invalid linesafter');
    }
    if (settings.linesBefore < 0) {
      throw SearchException('Invalid linesbefore');
    }
    if (settings.maxLineLength < 0) {
      throw SearchException('Invalid maxlinelength');
    }
    _textEncoding = Encoding.getByName(settings.textFileEncoding)!;
    if (_textEncoding == null) {
      throw SearchException(
          'Invalid or unsupported encoding: ${settings.textFileEncoding}');
    } else if (allowInvalid) {
      if (_textEncoding == utf8) {
        _textEncoding = Utf8Codec(allowMalformed: true);
      } else if (_textEncoding == latin1) {
        _textEncoding = Latin1Codec(allowInvalid: true);
      } else if (_textEncoding == ascii) {
        _textEncoding = AsciiCodec(allowInvalid: true);
      }
    }
  }

  bool _anyMatchesAnyPattern(Iterable<String> elems, Set<Pattern> patterns) {
    return elems.any((elem) => _matchesAnyPattern(elem, patterns));
  }

  bool _matchesAnyPattern(String s, Set<Pattern> patterns) {
    return patterns.any((p) => p.allMatches(s).isNotEmpty);
  }

  Future<List<SearchResult>> _searchBinaryFile(FileResult fr) async {
    if (settings.debug) {
      logMsg('Searching binary file $fr');
    }
    return fr.file
        .readAsString(encoding: _binaryEncoding)
        .then((String contents) {
      var results = <SearchResult>[];
      for (var p in settings.searchPatterns) {
        var matches = [];
        if (settings.firstMatch) {
          var match = (p as RegExp).firstMatch(contents);
          if (match != null) {
            matches = [match];
          }
        } else {
          matches = p.allMatches(contents).toList();
        }
        for (var m in matches) {
          results.add(
              SearchResult(p, fr, 0, m.start + 1, m.end + 1, null, [], []));
        }
      }
      return results;
    });
  }

  bool _linesMatch(
      List<String> lines, Set<Pattern> inPatterns, Set<Pattern> outPatterns) {
    return lines.isEmpty ||
        ((inPatterns.isEmpty || _anyMatchesAnyPattern(lines, inPatterns)) &&
            (outPatterns.isEmpty ||
                !_anyMatchesAnyPattern(lines, outPatterns)));
  }

  Future<List<SearchResult>> searchLineStream(Stream<String> stream) async {
    var results = <SearchResult>[];
    var lineNum = 0;
    var matchedPatterns = {};
    if (settings.firstMatch) {
      stream = stream.takeWhile((_) {
        return matchedPatterns.length < settings.searchPatterns.length;
      });
    }

    try {
      var linesBefore = <String>[];
      var linesAfter = <String>[];

      var it = StreamIterator(stream);

      while (true) {
        lineNum++;
        String line;
        if (linesAfter.isNotEmpty) {
          line = linesAfter.removeAt(0);
        } else if (await it.moveNext()) {
          line = it.current;
        } else {
          break;
        }
        if (settings.linesAfter > 0) {
          while (
              linesAfter.length < settings.linesAfter && await it.moveNext()) {
            linesAfter.add(it.current);
          }
        }

        var searchPatterns = <Pattern>{};
        if (settings.firstMatch) {
          searchPatterns = settings.searchPatterns
              .where((elem) => !matchedPatterns.containsKey(elem))
              .toSet();
        } else {
          searchPatterns = settings.searchPatterns;
        }
        if (searchPatterns.isEmpty) {
          break;
        }
        for (var p in searchPatterns) {
          var matches = [];
          if (settings.firstMatch) {
            var match = (p as RegExp).firstMatch(line);
            if (match != null) {
              matches = [match];
              matchedPatterns[p] = 1;
            }
          } else {
            matches = p.allMatches(line).toList();
          }

          if (matches.isEmpty ||
              !_linesMatch(linesBefore, settings.inLinesBeforePatterns,
                  settings.outLinesBeforePatterns) ||
              !_linesMatch(linesAfter, settings.inLinesAfterPatterns,
                  settings.outLinesAfterPatterns)) {
            continue;
          }

          for (var m in matches) {
            results.add(SearchResult(p, null, lineNum, m.start + 1, m.end + 1,
                line, linesBefore.toList(), linesAfter.toList()));
          }
        }

        if (settings.linesBefore > 0) {
          if (linesBefore.length == settings.linesBefore) {
            linesBefore.removeAt(0);
          }
          if (linesBefore.length < settings.linesBefore) {
            linesBefore.add(line);
          }
        }
        if (settings.firstMatch &&
            matchedPatterns.length == settings.searchPatterns.length) {
          break;
        }
      }

      return results;
    } catch (e) {
      logMsg(e.toString());
      return [];
    }
  }

  Future<List<SearchResult>> _searchTextFileLines(FileResult fr) async {
    var results = <SearchResult>[];
    var inputStream = fr.file.openRead();
    try {
      var lineStream =
          _textEncoding!.decoder.bind(inputStream).transform(LineSplitter());
      return searchLineStream(lineStream).then((results) {
        return results.map((r) {
          r.file = fr;
          return r;
        }).toList();
      });
    } on FormatException catch (e) {
      logMsg('Error reading ${fr.file}: ${e.message}');
    } catch (e) {
      logMsg(e.toString());
    }
    return results;
  }

  List<String> _getLinesFromMultilineString(
      String s, List<int> newLineIndices) {
    var lines = <String>[];
    for (var i = 0; i < newLineIndices.length - 1; ++i) {
      lines.add(s.substring(newLineIndices[i] + 1, newLineIndices[i + 1]));
    }
    return lines;
  }

  List<SearchResult> searchMultilineString(String s) {
    var results = <SearchResult>[];
    var newLineIndices = <int>[];
    var it = s.runes.iterator;
    var i = 0;
    while (it.moveNext()) {
      if (it.current == 10) {
        newLineIndices.add(i);
      }
      i++;
    }

    for (var p in settings.searchPatterns) {
      var matches = [];
      if (settings.firstMatch) {
        var match = (p as RegExp).firstMatch(s);
        if (match != null) {
          matches = [match];
        }
      } else {
        matches = p.allMatches(s).toList();
      }
      for (var m in matches) {
        var beforeNewlineIndices =
            newLineIndices.takeWhile((i) => i <= m.start).toList();
        var linesBefore = <String>[];
        if (settings.linesBefore > 0) {
          var linesBeforeIndices = beforeNewlineIndices.reversed
              .take(settings.linesBefore + 1)
              .toList()
              .reversed
              .toList();
          linesBefore = _getLinesFromMultilineString(s, linesBeforeIndices);
        }
        var afterNewlineIndices =
            newLineIndices.skipWhile((i) => i <= m.start).toList();
        var linesAfter = <String>[];
        if (settings.linesAfter > 0) {
          var linesAfterIndices =
              afterNewlineIndices.take(settings.linesAfter + 1).toList();
          linesAfter = _getLinesFromMultilineString(s, linesAfterIndices);
        }

        if (!_linesMatch(linesBefore, settings.inLinesBeforePatterns,
                settings.outLinesBeforePatterns) ||
            !_linesMatch(linesAfter, settings.inLinesAfterPatterns,
                settings.outLinesAfterPatterns)) {
          continue;
        }

        var lineNum = beforeNewlineIndices.length + 1;
        var startLineIndex = 0;
        if (lineNum > 1) {
          startLineIndex = beforeNewlineIndices.last + 1;
        }
        var endLineIndex = s.length - 1;
        if (afterNewlineIndices.isNotEmpty) {
          endLineIndex = afterNewlineIndices.first;
        }
        var line = s.substring(startLineIndex, endLineIndex);
        var startMatchIndex = m.start - startLineIndex + 1;
        var endMatchIndex = m.end - startLineIndex + 1;
        results.add(SearchResult(p, null, lineNum, startMatchIndex,
            endMatchIndex, line, linesBefore, linesAfter));
      }
    }

    return results;
  }

  Future<List<SearchResult>> _searchTextFileContents(FileResult fr) async {
    return fr.file
        .readAsString(encoding: _textEncoding!)
        .then((String contents) {
      var results = searchMultilineString(contents);
      return results.map((r) {
        r.file = fr;
        return r;
      }).toList();
    });
  }

  Future<List<SearchResult>> _searchTextFile(FileResult fr) async {
    if (settings.debug) {
      logMsg('Searching text file $fr');
    }
    if (settings.multiLineSearch) {
      return _searchTextFileContents(fr);
    } else {
      return _searchTextFileLines(fr);
    }
  }

  Future<List<SearchResult>> _searchFile(FileResult fr) async {
    var results = <SearchResult>[];
    if ({FileType.text, FileType.code, FileType.xml}.contains(fr.fileType)) {
      return _searchTextFile(fr);
    } else if (fr.fileType == FileType.binary) {
      return _searchBinaryFile(fr);
    }
    return results;
  }

  int cmpByMatchLocation(SearchResult r1, SearchResult r2) {
    if (r1.lineNum != r2.lineNum) {
      return r1.lineNum.compareTo(r2.lineNum);
    }
    if (r1.matchStartIndex != r2.matchStartIndex) {
      return r1.matchStartIndex.compareTo(r2.matchStartIndex);
    }
    return r1.matchEndIndex.compareTo(r2.matchEndIndex);
  }

  int cmpByFilePath(SearchResult r1, SearchResult r2) {
    var cmp = 0;
    if (r1.file != null && r2.file != null) {
      cmp = _finder.cmpByFilePath(r1.file!, r2.file!);
    }
    if (cmp != 0) {
      return cmp;
    }
    return cmpByMatchLocation(r1, r2);
  }

  int cmpByFileName(SearchResult r1, SearchResult r2) {
    var cmp = 0;
    if (r1.file != null && r2.file != null) {
      cmp = _finder.cmpByFileName(r1.file!, r2.file!);
    }
    if (cmp != 0) {
      return cmp;
    }
    return cmpByMatchLocation(r1, r2);
  }

  int cmpByFileSize(SearchResult r1, SearchResult r2) {
    var cmp = 0;
    if (r1.file != null && r2.file != null) {
      cmp = _finder.cmpByFileSize(r1.file!, r2.file!);
    }
    if (cmp != 0) {
      return cmp;
    }
    return cmpByMatchLocation(r1, r2);
  }

  int cmpByFileType(SearchResult r1, SearchResult r2) {
    var cmp = 0;
    if (r1.file != null && r2.file != null) {
      cmp = _finder.cmpByFileType(r1.file!, r2.file!);
    }
    if (cmp != 0) {
      return cmp;
    }
    return cmpByMatchLocation(r1, r2);
  }

  int cmpByLastMod(SearchResult r1, SearchResult r2) {
    var cmp = 0;
    if (r1.file != null && r2.file != null) {
      cmp = _finder.cmpByLastMod(r1.file!, r2.file!);
    }
    if (cmp != 0) {
      return cmp;
    }
    return cmpByMatchLocation(r1, r2);
  }

  int Function(SearchResult, SearchResult)? getSortComparator() {
    if (settings.sortDescending) {
      switch (settings.sortBy) {
        case SortBy.fileName:
          return (SearchResult r1, SearchResult r2) => cmpByFileName(r2, r1);
        case SortBy.fileSize:
          return (SearchResult r1, SearchResult r2) => cmpByFileSize(r2, r1);
        case SortBy.fileType:
          return (SearchResult r1, SearchResult r2) => cmpByFileType(r2, r1);
        case SortBy.lastMod:
          return (SearchResult r1, SearchResult r2) => cmpByLastMod(r2, r1);
        default:
          return (SearchResult r1, SearchResult r2) => cmpByFilePath(r2, r1);
      }
    }
    switch (settings.sortBy) {
      case SortBy.fileName:
        return (SearchResult r1, SearchResult r2) => cmpByFileName(r1, r2);
      case SortBy.fileSize:
        return (SearchResult r1, SearchResult r2) => cmpByFileSize(r1, r2);
      case SortBy.fileType:
        return (SearchResult r1, SearchResult r2) => cmpByFileType(r1, r2);
      case SortBy.lastMod:
        return (SearchResult r1, SearchResult r2) => cmpByLastMod(r1, r2);
      default:
        return (SearchResult r1, SearchResult r2) => cmpByFilePath(r1, r2);
    }
  }

  void sortSearchResults(List<SearchResult> results) {
    var sortComparator = getSortComparator();
    results.sort(sortComparator);
  }

  Future<List<SearchResult>> _searchFiles() async {
    var fileResults = await _finder.find();
    if (settings.verbose) {
      var searchDirs =
          fileResults.map((fr) => fr.file.parent.path).toSet().toList();
      logMsg('\nDirectories to be searched (${searchDirs.length}):');
      for (var d in searchDirs) {
        logMsg(FileUtil.contractPath(d));
      }
      logMsg('\nFiles to be searched (${fileResults.length}):');
      for (var fr in fileResults) {
        logMsg(FileUtil.contractPath(fr.file.path));
      }
    }
    // this is the (almost) largest batch size you can have before you get the
    // "too many files open" errors
    var batchSize = 245;
    var offset = 0;

    var results = <SearchResult>[];

    while (offset < fileResults.length) {
      var toIndex = min(offset + batchSize, fileResults.length);
      var fileResultsFutures =
          fileResults.sublist(offset, toIndex).map((sf) => _searchFile(sf));
      await Future.wait(fileResultsFutures).then((filesResults) {
        for (var fileResults in filesResults) {
          results.addAll(fileResults);
        }
      });
      offset += batchSize;
    }

    if (results.length > 1) {
      sortSearchResults(results);
    }
    return results;
  }

  Future<List<SearchResult>> search() async {
    return Future.wait([_fileTypes.ready]).then((res) {
      return _searchFiles();
    });
  }

  //////////////////////////////////////////////////////////////////////////////

  void printResults(
      List<SearchResult> results, SearchResultFormatter formatter) {
    if (results.isNotEmpty) {
      logMsg('\nSearch results (${results.length}):');
      for (var r in results) {
        logMsg(formatter.format(r));
      }
    } else {
      logMsg('\nSearch results: 0');
    }
  }

  int sortFileResults(FileResult fr1, FileResult fr2) {
    if (fr1.file.parent.path == fr2.file.parent.path) {
      return fr1.file.path.compareTo(fr2.file.path);
    } else {
      return fr1.file.parent.path.compareTo(fr2.file.parent.path);
    }
  }

  List<FileResult> getMatchingFileResults(List<SearchResult> results) {
    var files = results.map((r) => r.file!).toSet().toList();
    files.sort(sortFileResults);
    return files;
  }

  void printMatchingDirs(
      List<SearchResult> results, SearchResultFormatter formatter) {
    var files = getMatchingFileResults(results);
    _finder.printMatchingDirs(files, formatter.fileFormatter!);
  }

  void printMatchingFiles(
      List<SearchResult> results, SearchResultFormatter formatter) {
    var files = getMatchingFileResults(results);
    _finder.printMatchingFiles(files, formatter.fileFormatter!);
  }

  List<String> getMatchingLines(
      List<SearchResult> results, SearchSettings settings) {
    var lines = results
        .where((r) => r.line != null)
        .map((r) => r.line!.trim())
        .toList();
    if (settings.uniqueLines) {
      lines = lines.toSet().toList();
    }
    lines.sort((l1, l2) => l1.toLowerCase().compareTo(l2.toLowerCase()));
    return lines;
  }

  void printMatchingLines(
      List<SearchResult> results, SearchResultFormatter formatter) {
    var lines = getMatchingLines(results, settings);
    String msg;
    if (settings.uniqueLines) {
      msg = '\nUnique matching lines';
    } else {
      msg = '\nMatching lines';
    }
    if (lines.isNotEmpty) {
      logMsg('$msg (${lines.length}):');
      for (var line in lines) {
        logMsg(formatter.formatLine(line));
      }
    } else {
      logMsg('$msg: 0');
    }
  }
}
