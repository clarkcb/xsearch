import 'dart:convert' show json;
import 'dart:io' show File;

import 'package:dartsearch/src/config.dart' show FILETYPESPATH;
import 'package:dartsearch/src/file_util.dart';

enum FileType {
  unknown,
  archive,
  binary,
  code,
  text,
  xml,
}

extension FileTypeExtension on FileType {
  String get name {
    switch (this) {
      case FileType.text:
        return FileTypes.text;
      case FileType.code:
        return FileTypes.code;
      case FileType.xml:
        return FileTypes.xml;
      case FileType.binary:
        return FileTypes.binary;
      case FileType.archive:
        return FileTypes.archive;
      default:
        return FileTypes.unknown;
    }
  }
}

class FileTypes {
  static const archive = 'archive';
  static const binary = 'binary';
  static const code = 'code';
  static const searchable = 'searchable';
  static const text = 'text';
  static const xml = 'xml';
  static const unknown = 'unknown';

  var fileTypeMap = {};
  Future ready;

  FileTypes() {
    ready = loadFileTypesFromJson();
  }

  Future<void> loadFileTypesFromJson() async {
    var contents = await File(FILETYPESPATH).readAsString();
    Map jsonFileTypesMap = json.decode(contents);
    if (jsonFileTypesMap.containsKey('filetypes')) {
      var ftList = jsonFileTypesMap['filetypes'] as List;
      ftList.forEach((ft) {
        var typeName = (ft as Map)['type'];
        var extensions = (ft as Map)['extensions'].toSet();
        fileTypeMap[typeName] = extensions;
      });
      fileTypeMap[text] =
          fileTypeMap[text].union(fileTypeMap[code].union(fileTypeMap[xml]));
      fileTypeMap[searchable] =
          fileTypeMap[text].union(fileTypeMap[binary].union(fileTypeMap[archive]));
    }
  }

  static FileType fromName(String typeName) {
    switch (typeName.trim().toLowerCase()) {
      case FileTypes.text:
        return FileType.text;
        break;
      case FileTypes.code:
        return FileType.code;
        break;
      case FileTypes.xml:
        return FileType.xml;
        break;
      case FileTypes.binary:
        return FileType.binary;
        break;
      case FileTypes.archive:
        return FileType.archive;
        break;
      default:
        return FileType.unknown;
    }
  }

  Future<FileType> getFileType(String fileName) async {
    return await ready.then((_) {
      var ext = FileUtil.extension(fileName);
      if (fileTypeMap[code].contains(ext)) return FileType.code;
      if (fileTypeMap[xml].contains(ext)) return FileType.xml;
      if (fileTypeMap[text].contains(ext)) return FileType.text;
      if (fileTypeMap[binary].contains(ext)) return FileType.binary;
      if (fileTypeMap[archive].contains(ext)) return FileType.archive;
      return FileType.unknown;
    });
  }

  Future<bool> isArchiveFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeMap[archive].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isBinaryFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeMap[binary].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isCodeFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeMap[code].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isTextFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeMap[text].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isXmlFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeMap[xml].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isSearchableFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeMap[searchable].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isUnknownFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeMap[unknown].contains(FileUtil.extension(fileName)) ||
          !fileTypeMap[searchable].contains(FileUtil.extension(fileName));
    });
  }
}
