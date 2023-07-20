import 'dart:io' show File;

import 'package:test/test.dart';

void main() {
  test('test SearchFile with absolute path', () {
    var path = '/Users/cary/src/xsearch/dart/dartsearch/lib/src/search_file.dart';
    var searchFile = SearchFile(File(path), FileType.code);
    expect(searchFile.toString(), path);
  });

  test('test SearchFile with tilde path', () {
    var path = '~/src/xsearch/dart/dartsearch/lib/src/search_file.dart';
    var searchFile = SearchFile(File(path), FileType.code);
    expect(searchFile.toString(), path);
  });

  test('test SearchFile with rel path #1', () {
    var path = './search_file.dart';
    var searchFile = SearchFile(File(path), FileType.code);
    expect(searchFile.toString(), path);
  });

  test('test SearchFile with rel path #2', () {
    var path = './search_file.dart';
    var searchFile = SearchFile(File(path), FileType.code);
    expect(searchFile.toString(), path);
  });
}
