import 'package:dartsearch/src/file_types.dart';

class SearchSettings {
  bool _archivesOnly = false;
  bool get archivesOnly => _archivesOnly;
  set archivesOnly(bool value) {
    _archivesOnly = value;
    if (value) {
      searchArchives = value;
    }
  }
  bool colorize = true;
  bool _debug = false;
  bool get debug => _debug;
  set debug(bool value) {
    _debug = value;
    if (value) {
      verbose = value;
    }
  }
  bool excludeHidden = true;
  bool firstMatch = false;

  var inArchiveExtensions = <String>{};
  var inArchiveFilePatterns = <Pattern>{};
  var inDirPatterns = <Pattern>{};
  var inExtensions = <String>{};
  var inFilePatterns = <Pattern>{};

  var inFileTypes = <FileType>{};

  var inLinesAfterPatterns = <Pattern>{};
  var inLinesBeforePatterns = <Pattern>{};

  int linesAfter  = 0;
  var linesAfterToPatterns = <Pattern>{};
  var linesAfterUntilPatterns = <Pattern>{};
  int linesBefore  = 0;
  bool listDirs  = false;
  bool listFiles  = false;
  bool listLines  = false;
  int maxLineLength  = 150;
  bool multiLineSearch  = false;

  var outArchiveExtensions = <String>{};
  var outArchiveFilePatterns = <Pattern>{};
  var outDirPatterns = <Pattern>{};
  var outExtensions = <String>{};
  var outFilePatterns = <Pattern>{};

  var outFileTypes = <FileType>{};

  var outLinesAfterPatterns = <Pattern>{};
  var outLinesBeforePatterns = <Pattern>{};

  bool printResults  = false;
  bool printUsage  = false;
  bool printVersion  = false;
  bool recursive  = true;
  bool searchArchives  = false;

  var searchPatterns = <Pattern>{};

  String startPath  = '';
  String textFileEncoding  = 'UTF-8';
  bool uniqueLines  = false;
  bool verbose  = false;

  void addExtensions(String exts, Set<String> extensions) {
    var extList = exts.split(',').where((ext) => ext.isNotEmpty).toList();
    addExtensionsList(extList, extensions);
  }

  void addExtensionsList(List<String> exts, Set<String> extensions) {
    exts.forEach((ext) {
      extensions.add(ext);
    });
  }

  void addPattern(Pattern pattern, Set<Pattern> patterns) {
    patterns.add(RegExp(pattern, multiLine: true));
  }

  String patternSetToString(Set<Pattern> patterns) {
    return '{' + patterns.map((p) => '"${(p as RegExp).pattern}"').join(', ') + '}';
  }

  String stringSetToString(Set<String> set) {
    return '{' + set.map((s) => '"$s"').join(', ') + '}';
  }

  @override
  String toString() => 'SearchSettings(archivesOnly: $archivesOnly'
      ', colorize: $colorize'
      ', debug: $debug'
      ', excludeHidden: $excludeHidden'
      ', firstMatch: $firstMatch'
      ', inArchiveExtensions: ${stringSetToString(inArchiveExtensions)}'
      ', inArchiveFilePatterns: ${patternSetToString(inArchiveFilePatterns)}'
      ', inDirPatterns: ${patternSetToString(inDirPatterns)}'
      ', inExtensions: ${stringSetToString(inExtensions)}'
      ', inFilePatterns: ${patternSetToString(inFilePatterns)}'
      ', inFileTypes: $inFileTypes'
      ', inLinesAfterPatterns: ${patternSetToString(inLinesAfterPatterns)}'
      ', inLinesBeforePatterns: ${patternSetToString(inLinesBeforePatterns)}'
      ', linesAfter: $linesAfter'
      ', linesAfterToPatterns: ${patternSetToString(linesAfterToPatterns)}'
      ', linesAfterUntilPatterns: ${patternSetToString(linesAfterUntilPatterns)}'
      ', linesBefore: $linesBefore'
      ', listDirs: $listDirs'
      ', listFiles: $listFiles'
      ', listLines: $listLines'
      ', maxLineLength: $maxLineLength'
      ', multiLineSearch: $multiLineSearch'
      ', outArchiveExtensions: ${stringSetToString(outArchiveExtensions)}'
      ', outArchiveFilePatterns: ${patternSetToString(outArchiveFilePatterns)}'
      ', outDirPatterns: ${patternSetToString(outDirPatterns)}'
      ', outExtensions: ${stringSetToString(outExtensions)}'
      ', outFilePatterns: ${patternSetToString(outFilePatterns)}'
      ', outFileTypes: $outFileTypes'
      ', outLinesAfterPatterns: ${patternSetToString(outLinesAfterPatterns)}'
      ', outLinesBeforePatterns: ${patternSetToString(outLinesBeforePatterns)}'
      ', printResults: $printResults'
      ', printUsage: $printUsage'
      ', printVersion: $printVersion'
      ', recursive: $recursive'
      ', searchArchives: $searchArchives'
      ', searchPatterns: ${patternSetToString(searchPatterns)}'
      ', startPath: "$startPath"'
      ', textFileEncoding: "$textFileEncoding"'
      ', uniqueLines: $uniqueLines'
      ', verbose: $verbose'
      ')';
}
