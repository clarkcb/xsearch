import 'package:dartsearch/src/console_color.dart';
import 'package:dartsearch/src/search_file.dart';
import 'package:dartsearch/src/search_settings.dart';

class SearchResult {
  final Pattern searchPattern;
  SearchFile file;
  final int lineNum;
  final int matchStartIndex;
  final int matchEndIndex;
  final String line;
  final List<String> linesBefore;
  final List<String> linesAfter;

  SearchResult(
      this.searchPattern,
      this.file,
      this.lineNum,
      this.matchStartIndex,
      this.matchEndIndex,
      this.line,
      this.linesBefore,
      this.linesAfter);
}

class SearchResultFormatter {
  static const noSearchFileText = '<text>';

  final SearchSettings settings;

  const SearchResultFormatter(this.settings);

  String colorize(String s, int matchStartIndex, int matchEndIndex) {
    return s.substring(0, matchStartIndex) +
        ConsoleColor.GREEN +
        s.substring(matchStartIndex, matchEndIndex) +
        ConsoleColor.RESET +
        s.substring(matchEndIndex);
  }

  int _lineNumPadding(SearchResult result) {
    var maxLineNum = result.lineNum + result.linesAfter.length;
    return '$maxLineNum'.length;
  }

  String _multiLineFormat(SearchResult result) {
    const lineSepLength = 80;

    String filePath;
    if (result.file == null) {
      filePath = noSearchFileText;
      // TODO: associate path parameters with results
      // } else if (settings.startPath.startsWith('~')) {
      //   filePath = FileUtil.contractPath(result.file.file.path);
    } else {
      filePath = result.file.file.path;
    }
    var s = ('=' * lineSepLength) +
        '\n' +
        filePath +
        ': ' +
        '${result.lineNum}: [${result.matchStartIndex}:${result.matchEndIndex}]\n' +
        ('-' * lineSepLength) +
        '\n';
    var currentLineNum = result.lineNum;
    var lineNumPadding = _lineNumPadding(result);
    if (result.linesBefore.isNotEmpty) {
      currentLineNum -= result.linesBefore.length;
      for (var lineBefore in result.linesBefore) {
        var lineNumString = '$currentLineNum'.padLeft(lineNumPadding);
        s += '  $lineNumString | $lineBefore\n';
        currentLineNum++;
      }
    }
    var line = result.line;
    if (settings.colorize) {
      line =
          colorize(line, result.matchStartIndex - 1, result.matchEndIndex - 1);
    }
    var lineNumString = '$currentLineNum'.padLeft(lineNumPadding);
    s += '> $lineNumString | $line\n';
    if (result.linesAfter.isNotEmpty) {
      currentLineNum++;
      for (var lineAfter in result.linesAfter) {
        var lineNumString = '$currentLineNum'.padLeft(lineNumPadding);
        s += '  $lineNumString | $lineAfter\n';
        currentLineNum++;
      }
    }
    return s;
  }

  String _formatMatchingLine(SearchResult result) {
    var formatted = result.line.trim();
    var leadingWhitespaceCount =
        result.line.trimRight().length - formatted.length;
    var formattedLength = formatted.length;
    var maxLineEndIndex = formattedLength - 1;
    var matchLength = result.matchEndIndex - result.matchStartIndex;
    var matchStartIndex = result.matchStartIndex - 1 - leadingWhitespaceCount;
    var matchEndIndex = matchStartIndex + matchLength;

    if (formattedLength > settings.maxLineLength) {
      var lineStartIndex = matchStartIndex;
      var lineEndIndex = lineStartIndex + matchLength;
      matchStartIndex = 0;
      matchEndIndex = matchLength;

      while (lineEndIndex > formattedLength - 1) {
        lineStartIndex--;
        matchStartIndex++;
        matchEndIndex++;
      }

      formattedLength = lineEndIndex - lineStartIndex;
      while (formattedLength < settings.maxLineLength) {
        if (lineStartIndex > 0) {
          lineStartIndex--;
          matchStartIndex++;
          matchEndIndex++;
          formattedLength = lineEndIndex - lineStartIndex;
        }
        if (formattedLength < settings.maxLineLength &&
            lineEndIndex < maxLineEndIndex) {
          lineEndIndex++;
        }
        formattedLength = lineEndIndex - lineStartIndex;
      }

      formatted = formatted.substring(lineStartIndex, lineEndIndex);

      if (lineStartIndex > 2) {
        formatted = '...' + formatted.substring(3);
      }
      if (lineEndIndex < maxLineEndIndex - 3) {
        formatted = formatted.substring(0, formattedLength - 3) + '...';
      }
    }

    if (settings.colorize) {
      formatted = colorize(formatted, matchStartIndex, matchEndIndex);
    }

    return formatted;
  }

  String _singleLineFormat(SearchResult result) {
    String s;
    if (result.file == null) {
      s = noSearchFileText;
      // TODO: associate path parameters with results
      // } else if (settings.startPath.startsWith('~')) {
      //   s = FileUtil.contractPath(result.file.file.path);
    } else {
      s = result.file.file.path;
    }
    if (result.lineNum == 0) {
      s += ' matches at [${result.matchStartIndex}:${result.matchEndIndex}]';
    } else {
      s +=
          ': ${result.lineNum}: [${result.matchStartIndex}:${result.matchEndIndex}]: ';
      s += _formatMatchingLine(result);
    }
    return s;
  }

  String format(SearchResult result) {
    if ((settings.linesBefore > 0 || settings.linesAfter > 0) &&
        result.lineNum > 0) {
      return _multiLineFormat(result);
    }
    return _singleLineFormat(result);
  }
}
