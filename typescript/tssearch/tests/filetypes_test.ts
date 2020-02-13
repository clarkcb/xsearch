/*
 * filetypes_test.js
 *
 * Some nodeunit tests of filetypes.js
 */

import {FileType} from '../src/filetype';
import {FileTypes} from '../src/filetypes';

exports.testFileTypesArchiveFile = function(test) {
    const filename: string = 'archive.zip';
    const res: boolean = FileTypes.isArchiveFile(filename);
    test.ok(res, filename + " is archive file");
    const type: FileType = FileTypes.getFileType(filename);
    test.ok(type === FileType.Archive, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesBinaryFile = function(test) {
    const filename: string = 'binary.exe';
    const res: boolean = FileTypes.isBinaryFile(filename);
    test.ok(res, filename + " is binary file");
    const type: FileType = FileTypes.getFileType(filename);
    test.ok(type === FileType.Binary, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesTextFile = function(test) {
    const filename: string = 'text.txt';
    const res: boolean = FileTypes.isTextFile(filename);
    test.ok(res, filename + " is text file");
    const type: FileType = FileTypes.getFileType(filename);
    test.ok(type === FileType.Text, "FileType of " + filename + " is " + type);
    test.done();
};

exports.testFileTypesUnknownFile = function(test) {
    const filename: string = 'unknown.xyz';
    const res: boolean = FileTypes.isUnknownFile(filename);
    test.ok(res, filename + " is unknown file");
    const type: FileType = FileTypes.getFileType(filename);
    test.ok(type === FileType.Unknown, "FileType of " + filename + " is " + type);
    test.done();
};
