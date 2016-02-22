/// <reference path="../typings/node/node.d.ts"/>
/// <reference path="../src/filetype.ts"/>
/// <reference path="../src/filetypes.ts"/>
/*
 * filetypes_test.js
 *
 * Some nodeunit tests of filetypes.js
 */
var FileType = require('../build/filetype.js').FileType;
var FileTypes = require('../build/filetypes.js').FileTypes;
exports.testFileTypesArchiveFile = function (test) {
    var filename = 'archive.zip';
    var res = FileTypes.isArchiveFile(filename);
    test.ok(res, filename + " is archive file");
    var type = FileTypes.getFileType(filename);
    test.ok(type === FileType.Archive, "FileType of " + filename + " is " + type);
    test.done();
};
exports.testFileTypesBinaryFile = function (test) {
    var filename = 'binary.exe';
    var res = FileTypes.isBinaryFile(filename);
    test.ok(res, filename + " is binary file");
    var type = FileTypes.getFileType(filename);
    test.ok(type === FileType.Binary, "FileType of " + filename + " is " + type);
    test.done();
};
exports.testFileTypesTextFile = function (test) {
    var filename = 'text.txt';
    var res = FileTypes.isTextFile(filename);
    test.ok(res, filename + " is text file");
    var type = FileTypes.getFileType(filename);
    test.ok(type === FileType.Text, "FileType of " + filename + " is " + type);
    test.done();
};
exports.testFileTypesUnknownFile = function (test) {
    var filename = 'unknown.xyz';
    var res = FileTypes.isUnknownFile(filename);
    test.ok(res, filename + " is unknown file");
    var type = FileTypes.getFileType(filename);
    test.ok(type === FileType.Unknown, "FileType of " + filename + " is " + type);
    test.done();
};
