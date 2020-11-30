/*jshint esnext: true */
/*
 * filetype.js
 *
 * FileType "static" class
 *
 */

const FileType = {
    UNKNOWN: 1,
    ARCHIVE: 2,
    BINARY:  3,
    CODE:    4,
    TEXT:    5,
    XML:     6
};
Object.freeze(FileType);

exports.FileType = FileType;
