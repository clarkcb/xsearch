import 'package:dartfind/dartfind.dart'
    show Color, FileResult, FileResultFormatter, FileResultSorter, SortBy;
import 'package:dartsearch/src/search_settings.dart';

class SearchResult {
  final Pattern searchPattern;
  FileResult? file;
  final int lineNum;
  final int matchStartIndex;
  final int matchEndIndex;
  final String? line;
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
  FileResultFormatter? fileFormatter;
  String Function(String) formatLine = (String line) => line;
  String Function(String) formatMatch = (String match) => match;

  SearchResultFormatter(this.settings) {
    fileFormatter = FileResultFormatter(settings);
    if (settings.colorize) {
      formatLine = formatLineWithColor;
      formatMatch = formatMatchWithColor;
    }
  }

  String colorize(
      String s, int matchStartIndex, int matchEndIndex, Color color) {
    return fileFormatter!.colorize(s, matchStartIndex, matchEndIndex, color);
  }

  String formatLineWithColor(String line) {
    var formattedLine = line;
    for (var p in settings.searchPatterns) {
      var match = (p as RegExp).firstMatch(formattedLine);
      if (match != null) {
        formattedLine =
            colorize(formattedLine, match.start, match.end, settings.lineColor);
        break;
      }
    }
    return formattedLine;
  }

  String formatMatchWithColor(String match) {
    return colorize(match, 0, match.length, settings.lineColor);
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
      filePath = fileFormatter!.formatFileResult(result.file!);
    }
    var s = '${'=' * lineSepLength}\n';
    s += '$filePath: ${result.lineNum}:';
    s += ' [${result.matchStartIndex}:${result.matchEndIndex}]\n';
    s += '${'-' * lineSepLength}\n';
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
    var line = '';
    if (result.line != null) {
      line = result.line!;
    }
    if (settings.colorize) {
      line = colorize(line, result.matchStartIndex - 1,
          result.matchEndIndex - 1, settings.lineColor);
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

  String _formatResultMatch(SearchResult result) {
    if (result.line == null ||
        result.line!.trim() == "" ||
        settings.maxLineLength == 0) {
      return "";
    }

    var matchLength = result.matchEndIndex - result.matchStartIndex;
    var matchStartIndex = result.matchStartIndex - 1;
    var matchEndIndex = matchStartIndex + matchLength;

    var prefix = '';
    var suffix = '';
    var colorStartIndex = 0;
    var colorEndIndex = matchLength;

    if (matchLength > settings.maxLineLength) {
      if (matchStartIndex > 2) {
        prefix = '...';
      }
      suffix = '...';
      colorStartIndex = prefix.length;
      colorEndIndex = settings.maxLineLength - suffix.length;
      matchEndIndex = matchStartIndex + colorEndIndex;
      matchStartIndex = matchStartIndex + colorStartIndex;
    }

    var matchString = prefix +
        result.line!.substring(matchStartIndex, matchEndIndex) +
        suffix;

    if (settings.colorize) {
      matchString = colorize(
          matchString, colorStartIndex, colorEndIndex, settings.lineColor);
    }

    return matchString;
  }

  String _formatResultLine(SearchResult result) {
    if (result.line == null ||
        result.line!.trim() == "" ||
        settings.maxLineLength == 0) {
      return '';
    }

    var maxLimit = settings.maxLineLength > 0;

    if (maxLimit &&
        result.matchEndIndex - result.matchStartIndex >
            settings.maxLineLength) {
      return _formatResultMatch(result);
    }

    var trimmed = result.line!.trim();
    var lineStartIndex = result.line!.trimRight().length - trimmed.length;
    var lineEndIndex = result.line!.trimRight().length - 1;
    var matchLength = result.matchEndIndex - result.matchStartIndex;
    var matchStartIndex = result.matchStartIndex - 1 - lineStartIndex;
    var matchEndIndex = matchStartIndex + matchLength;

    var prefix = '';
    var suffix = '';

    if (maxLimit && trimmed.length > settings.maxLineLength) {
      lineStartIndex = result.matchStartIndex - 1;
      lineEndIndex = lineStartIndex + matchLength;
      matchStartIndex = 0;
      matchEndIndex = matchLength;

      var currentLen = lineEndIndex - lineStartIndex;
      while (currentLen < settings.maxLineLength) {
        if (lineStartIndex > 0) {
          lineStartIndex--;
          matchStartIndex++;
          matchEndIndex++;
          currentLen++;
        }
        if (currentLen < settings.maxLineLength &&
            lineEndIndex < trimmed.length) {
          lineEndIndex++;
          currentLen++;
        }
      }

      if (lineStartIndex > 2) {
        prefix = '...';
        lineStartIndex += 3;
      }
      if (lineEndIndex < trimmed.length - 3) {
        suffix = '...';
        lineEndIndex -= 3;
      }
    } else {
      lineEndIndex++;
    }

    var formatted =
        prefix + result.line!.substring(lineStartIndex, lineEndIndex) + suffix;

    if (settings.colorize) {
      formatted = colorize(
          formatted, matchStartIndex, matchEndIndex, settings.lineColor);
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
      s = fileFormatter!.formatFileResult(result.file!);
    }
    if (result.lineNum == 0) {
      s += ' matches at [${result.matchStartIndex}:${result.matchEndIndex}]';
    } else {
      s +=
          ': ${result.lineNum}: [${result.matchStartIndex}:${result.matchEndIndex}]: ';
      s += _formatResultLine(result);
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

class SearchResultSorter {
  final SearchSettings settings;
  FileResultSorter? fileResultSorter;

  SearchResultSorter(this.settings) {
    fileResultSorter = FileResultSorter(settings);
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
      cmp = fileResultSorter!.cmpByFilePath(r1.file!, r2.file!);
    }
    if (cmp != 0) {
      return cmp;
    }
    return cmpByMatchLocation(r1, r2);
  }

  int cmpByFileName(SearchResult r1, SearchResult r2) {
    var cmp = 0;
    if (r1.file != null && r2.file != null) {
      cmp = fileResultSorter!.cmpByFileName(r1.file!, r2.file!);
    }
    if (cmp != 0) {
      return cmp;
    }
    return cmpByMatchLocation(r1, r2);
  }

  int cmpByFileSize(SearchResult r1, SearchResult r2) {
    var cmp = 0;
    if (r1.file != null && r2.file != null) {
      cmp = fileResultSorter!.cmpByFileSize(r1.file!, r2.file!);
    }
    if (cmp != 0) {
      return cmp;
    }
    return cmpByMatchLocation(r1, r2);
  }

  int cmpByFileType(SearchResult r1, SearchResult r2) {
    var cmp = 0;
    if (r1.file != null && r2.file != null) {
      cmp = fileResultSorter!.cmpByFileType(r1.file!, r2.file!);
    }
    if (cmp != 0) {
      return cmp;
    }
    return cmpByMatchLocation(r1, r2);
  }

  int cmpByLastMod(SearchResult r1, SearchResult r2) {
    var cmp = 0;
    if (r1.file != null && r2.file != null) {
      cmp = fileResultSorter!.cmpByLastMod(r1.file!, r2.file!);
    }
    if (cmp != 0) {
      return cmp;
    }
    return cmpByMatchLocation(r1, r2);
  }

  int Function(SearchResult, SearchResult)? getSearchResultComparator() {
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

  void sort(List<SearchResult> searchResults) {
    var searchResultComparator = getSearchResultComparator();
    searchResults.sort(searchResultComparator);
  }
}
