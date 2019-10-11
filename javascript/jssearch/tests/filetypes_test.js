/*
 * filetypes_test.js
 *
 * Some nodeunit tests of filetypes.js
 */

var FileType = require('../src/filetype.js').FileType;
var FileTypes = require('../src/filetypes.js').FileTypes;

exports.testFileTypesArchiveFile = function(test) {
    const fileTypes = new FileTypes();
    const filename = 'archive.zip';
    const res = fileTypes.isArchiveFile(filename);
    test.ok(res, filename + " is archive file");
    const type = fileTypes.getFileType(filename);
    test.ok(type === FileType.ARCHIVE, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesBinaryFile = function(test) {
    const fileTypes = new FileTypes();
    const filename = 'binary.exe';
    const res = fileTypes.isBinaryFile(filename);
    test.ok(res, filename + " is binary file");
    const type = fileTypes.getFileType(filename);
    test.ok(type === FileType.BINARY, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesTextFile = function(test) {
    const fileTypes = new FileTypes();
    const filename = 'text.txt';
    const res = fileTypes.isTextFile(filename);
    test.ok(res, filename + " is text file");
    const type = fileTypes.getFileType(filename);
    test.ok(type === FileType.TEXT, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesUnknownFile = function(test) {
    const fileTypes = new FileTypes();
    const filename = 'unknown.xyz';
    const res = fileTypes.isUnknownFile(filename);
    test.ok(res, filename + " is unknown file");
    const type = fileTypes.getFileType(filename);
    test.ok(type === FileType.UNKNOWN, "FileType of " + filename + " is " + type);
    test.done();
};
