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

exports.testFileTypesArchiveFile = function(test) {
    var filename: string = 'archive.zip';
    var res: boolean = FileTypes.isArchiveFile(filename);
    test.ok(res, filename + " is archive file");
    var type: FileType = FileTypes.getFileType(filename);
    test.ok(type === FileType.Archive, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesBinaryFile = function(test) {
    var filename: string = 'binary.exe';
    var res: boolean = FileTypes.isBinaryFile(filename);
    test.ok(res, filename + " is binary file");
    var type: FileType = FileTypes.getFileType(filename);
    test.ok(type === FileType.Binary, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesTextFile = function(test) {
    var filename: string = 'text.txt';
    var res: boolean = FileTypes.isTextFile(filename);
    test.ok(res, filename + " is text file");
    var type: FileType = FileTypes.getFileType(filename);
    test.ok(type === FileType.Text, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesUnknownFile = function(test) {
    var filename: string = 'unknown.xyz';
    var res: boolean = FileTypes.isUnknownFile(filename);
    test.ok(res, filename + " is unknown file");
    var type: FileType = FileTypes.getFileType(filename);
    test.ok(type === FileType.Unknown, "FileType of " + filename + " is " + type);
    test.done();
};
