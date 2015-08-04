/*
 * filetypes_test.js
 *
 * Some nodeunit tests of filetypes.js
 */

var FileType = require('../nodesearch/filetype.js').FileType;
var FileTypes = require('../nodesearch/filetypes.js').FileTypes;

exports.testFileTypesArchiveFile = function(test) {
    var fileTypes = new FileTypes();
    var filename = 'archive.zip';
    var res = fileTypes.isArchiveFile(filename);
    test.ok(res, filename + " is archive file");
    type = fileTypes.getFileType(filename);
    test.ok(type === FileType.ARCHIVE, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesBinaryFile = function(test) {
    var fileTypes = new FileTypes();
    var filename = 'binary.exe';
    var res = fileTypes.isBinaryFile(filename);
    test.ok(res, filename + " is binary file");
    type = fileTypes.getFileType(filename);
    test.ok(type === FileType.BINARY, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesTextFile = function(test) {
    var fileTypes = new FileTypes();
    var filename = 'text.txt';
    var res = fileTypes.isTextFile(filename);
    test.ok(res, filename + " is text file");
    type = fileTypes.getFileType(filename);
    test.ok(type === FileType.TEXT, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesUnknownFile = function(test) {
    var fileTypes = new FileTypes();
    var filename = 'unknown.xyz';
    var res = fileTypes.isUnknownFile(filename);
    test.ok(res, filename + " is unknown file");
    type = fileTypes.getFileType(filename);
    test.ok(type === FileType.UNKNOWN, "FileType of " + filename + " is " + type);
    test.done();
};
