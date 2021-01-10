import 'dart:io' show Platform;

import 'package:path/path.dart' as path;

class FileUtil {
  static final dotDirs = {'.', '..'};

  static String extension(String filePath) {
    var ext = path.extension(filePath);
    if (ext.isNotEmpty && ext.startsWith('.')) {
      ext = ext.substring(1);
    }
    return ext;
  }

  static String homePath() {
    if (Platform.isWindows) {
      return path.absolute(Platform.environment['USERPROFILE']);
    } else {
      return path.absolute(Platform.environment['HOME']);
    }
  }

  static String expandPath(String filePath) {
    if (filePath.startsWith('~')) {
      var homePath = FileUtil.homePath();
      return filePath.replaceFirst('~', homePath);
    }
    return filePath;
  }

  static String contractPath(String filePath) {
    var homePath = FileUtil.homePath();
    if (filePath.startsWith(homePath)) {
      return filePath.replaceFirst(homePath, '~');
    }
    return filePath;
  }

  static bool isDotDir(String filePath) => dotDirs.contains(filePath);

  static bool isHidden(String filePath) {
    var elems = filePath.split(Platform.pathSeparator)
        .where((e) => e.isNotEmpty).toSet();
    return elems.any((elem) => elem.startsWith('.') && !isDotDir(elem));
  }

  static String normalizePath(String filePath) {
    if (filePath.endsWith(Platform.pathSeparator)) {
      return filePath.substring(0, filePath.length - 2);
    }
    return filePath;
  }
}
