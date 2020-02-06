/*
 * filetypes_test.js
 *
 * Some nodeunit tests of filetypes.js
 */

const FileType = require('../src/filetype.js').FileType;
const FileTypes = require('../src/filetypes.js').FileTypes;

exports.testFileTypesArchiveFile = (test) => {
    const fileTypes = new FileTypes();
    const filename = 'archive.zip';
    const res = fileTypes.isArchiveFile(filename);
    test.ok(res, filename + " is archive file");
    const type = fileTypes.getFileType(filename);
    test.ok(type === FileType.ARCHIVE, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesBinaryFile = (test) => {
    const fileTypes = new FileTypes();
    const filename = 'binary.exe';
    const res = fileTypes.isBinaryFile(filename);
    test.ok(res, filename + " is binary file");
    const type = fileTypes.getFileType(filename);
    test.ok(type === FileType.BINARY, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesCodeFile = (test) => {
    const fileTypes = new FileTypes();
    const filename = 'code.js';
    const res = fileTypes.isCodeFile(filename);
    test.ok(res, filename + " is code file");
    const type = fileTypes.getFileType(filename);
    test.ok(type === FileType.CODE, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesTextFile = (test) => {
    const fileTypes = new FileTypes();
    const filename = 'text.txt';
    const res = fileTypes.isTextFile(filename);
    test.ok(res, filename + " is text file");
    const type = fileTypes.getFileType(filename);
    test.ok(type === FileType.TEXT, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesXmlFile = (test) => {
    const fileTypes = new FileTypes();
    const filename = 'markup.xml';
    const res = fileTypes.isXmlFile(filename);
    test.ok(res, filename + " is xml file");
    const type = fileTypes.getFileType(filename);
    test.ok(type === FileType.XML, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesSearchableFile = (test) => {
    const fileTypes = new FileTypes();
    const filename = 'README.md';
    const res = fileTypes.isSearchableFile(filename);
    test.ok(res, filename + " is searchable file");
    const type = fileTypes.getFileType(filename);
    test.ok(type === FileType.TEXT, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesUnknownFile = (test) => {
    const fileTypes = new FileTypes();
    const filename = 'unknown.xyz';
    const res = fileTypes.isUnknownFile(filename);
    test.ok(res, filename + " is unknown file");
    const type = fileTypes.getFileType(filename);
    test.ok(type === FileType.UNKNOWN, "FileType of " + filename + " is " + type);
    test.done();
};
