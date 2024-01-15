import 'package:dartfind/dartfind.dart' show FindSettings;

class SearchSettings extends FindSettings {
  bool colorize = true;
  bool firstMatch = false;
  var inLinesAfterPatterns = <Pattern>{};
  var inLinesBeforePatterns = <Pattern>{};
  int linesAfter = 0;
  var linesAfterToPatterns = <Pattern>{};
  var linesAfterUntilPatterns = <Pattern>{};
  int linesBefore = 0;
  bool listLines = false;
  int maxLineLength = 150;
  bool multiLineSearch = false;
  var outLinesAfterPatterns = <Pattern>{};
  var outLinesBeforePatterns = <Pattern>{};
  bool printResults = false;
  bool searchArchives = false;
  var searchPatterns = <Pattern>{};
  String textFileEncoding = 'UTF-8';
  bool uniqueLines = false;

  @override
  String toString() => 'SearchSettings(archivesOnly: $archivesOnly'
      ', colorize: $colorize'
      ', debug: $debug'
      ', firstMatch: $firstMatch'
      ', inArchiveExtensions: ${stringSetToString(inArchiveExtensions)}'
      ', inArchiveFilePatterns: ${patternSetToString(inArchiveFilePatterns)}'
      ', includeHidden: $includeHidden'
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
      ', maxDepth=$maxDepth'
      ', maxLastMod=${dateTimeToString(maxLastMod)}'
      ', maxLineLength: $maxLineLength'
      ', maxSize=$maxSize'
      ', minDepth=$minDepth'
      ', minLastMod=${dateTimeToString(minLastMod)}'
      ', minSize=$minSize'
      ', multiLineSearch: $multiLineSearch'
      ', outArchiveExtensions: ${stringSetToString(outArchiveExtensions)}'
      ', outArchiveFilePatterns: ${patternSetToString(outArchiveFilePatterns)}'
      ', outDirPatterns: ${patternSetToString(outDirPatterns)}'
      ', outExtensions: ${stringSetToString(outExtensions)}'
      ', outFilePatterns: ${patternSetToString(outFilePatterns)}'
      ', outFileTypes: $outFileTypes'
      ', outLinesAfterPatterns: ${patternSetToString(outLinesAfterPatterns)}'
      ', outLinesBeforePatterns: ${patternSetToString(outLinesBeforePatterns)}'
      ', paths: ${stringSetToString(paths)}'
      ', printResults: $printResults'
      ', printUsage: $printUsage'
      ', printVersion: $printVersion'
      ', recursive: $recursive'
      ', searchArchives: $searchArchives'
      ', searchPatterns: ${patternSetToString(searchPatterns)}'
      ', sortBy=${sortBy.name}'
      ', sortCaseInsensitive=$sortCaseInsensitive'
      ', sortDescending=$sortDescending'
      ', textFileEncoding: "$textFileEncoding"'
      ', uniqueLines: $uniqueLines'
      ', verbose: $verbose'
      ')';
}
