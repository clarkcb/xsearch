/*
 * filetypes.js
 *
 * identifies file types (archive, binary, text, unknown)
 */

const common = require('./common');
const config = require('./config');
const FileType = require('./filetype');
const {expandPath, getExtension} = require('./fileutil');

class FileTypes {
    "use strict";

    constructor() {
        this.fileTypeMap = (() => {
            let fs = require('fs');

            let json = '';
            if (fs.existsSync(expandPath(config.FILETYPESJSONPATH))) {
                json = fs.readFileSync(expandPath(config.FILETYPESJSONPATH)).toString();
            } else {
                throw new Error('File not found: ' + config.FILETYPESJSONPATH);
            }

            let fileTypeMap = {};

            let obj = JSON.parse(json);
            if (obj.hasOwnProperty('filetypes') && Array.isArray(obj['filetypes'])) {
                obj['filetypes'].forEach(ft => {
                    let typename = ft['type'];
                    let extensions = ft['extensions'];
                    fileTypeMap[typename] = common.setFromArray(extensions);
                });
            } else throw new Error("Invalid filetypes file: " + config.FILETYPESJSONPATH);

            fileTypeMap.text = [].concat(fileTypeMap.text, fileTypeMap.code, fileTypeMap.xml);
            fileTypeMap.searchable = [].concat(fileTypeMap.text, fileTypeMap.binary,
                fileTypeMap.archive);

            return fileTypeMap;
        })();
    }

    getFileType(filename) {
        if (this.isCodeFile(filename))
            return FileType.CODE;
        if (this.isXmlFile(filename))
            return FileType.XML;
        if (this.isTextFile(filename))
            return FileType.TEXT;
        if (this.isBinaryFile(filename))
            return FileType.BINARY;
        if (this.isArchiveFile(filename))
            return FileType.ARCHIVE;
        return FileType.UNKNOWN;
    }

    getFileTypeAsync(filename, cb) {
        console.log(filename);
        try {
            if (this.isCodeFile(filename))
                return cb(null, FileType.CODE);
            if (this.isXmlFile(filename))
                return cb(null, FileType.XML);
            if (this.isTextFile(filename))
                return cb(null, FileType.TEXT);
            if (this.isBinaryFile(filename))
                return cb(null, FileType.BINARY);
            if (this.isArchiveFile(filename))
                return cb(null, FileType.ARCHIVE);
        } catch (err) {
            return cb(err);
        }
        return cb(null, FileType.UNKNOWN);
    }

    isArchiveFile(filename) {
        let ext = getExtension(filename);
        return this.fileTypeMap.archive.indexOf(ext) > -1;
    }

    isBinaryFile(filename) {
        let ext = getExtension(filename);
        return this.fileTypeMap.binary.indexOf(ext) > -1;
    }

    isCodeFile(filename) {
        let ext = getExtension(filename);
        return this.fileTypeMap.code.indexOf(ext) > -1;
    }

    isSearchableFile(filename) {
        let ext = getExtension(filename);
        return this.fileTypeMap.searchable.indexOf(ext) > -1;
    }

    isTextFile(filename) {
        let ext = getExtension(filename);
        return this.fileTypeMap.text.indexOf(ext) > -1;
    }

    isXmlFile(filename) {
        let ext = getExtension(filename);
        return this.fileTypeMap.xml.indexOf(ext) > -1;
    }

    isUnknownFile(filename) {
        let ext = getExtension(filename);
        return this.fileTypeMap.searchable.indexOf(ext) === -1;
    }
}

FileTypes.fromName = (name) => {
    if (name.toUpperCase() === 'TEXT')
        return FileType.TEXT;
    if (name.toUpperCase() === 'BINARY')
        return FileType.BINARY;
    if (name.toUpperCase() === 'ARCHIVE')
        return FileType.ARCHIVE;
    if (name.toUpperCase() === 'CODE')
        return FileType.CODE;
    if (name.toUpperCase() === 'XML')
        return FileType.XML;
    return FileType.UNKNOWN;
};

FileTypes.toName = (fileType) => {
    if (fileType === FileType.ARCHIVE)
        return 'ARCHIVE';
    if (fileType === FileType.BINARY)
        return 'BINARY';
    if (fileType === FileType.CODE)
        return 'CODE';
    if (fileType === FileType.TEXT)
        return 'TEXT';
    if (fileType === FileType.XML)
        return 'XML';
    return 'UNKNOWN';
};

exports.FileTypes = FileTypes;
