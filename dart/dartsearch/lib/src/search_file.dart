import 'dart:io' show File;

import 'package:dartsearch/src/file_types.dart';

class SearchFile {
  static const String CONTAINER_SEPARATOR = '!';

  List<String> containers = [];
  final File file;
  final FileType fileType;

  SearchFile(this.file, this.fileType);

  @override
  String toString() {
    var s ='';
    if (containers.isNotEmpty) {
      s = containers.join(CONTAINER_SEPARATOR) + CONTAINER_SEPARATOR;
    }
    s += file.path;
    return s;
  }
}
