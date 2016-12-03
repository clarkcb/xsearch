/// <reference path="../typings/node/node.d.ts"/>
/// <reference path="filetype.ts"/>
/*
 * filetypes.ts
 *
 * identifies file types (archive, binary, text, unknown)
 */

"use strict";

var fs = require('fs');

var common = require('./common.ts');
var config = require('./config.ts');
var FileType = require('./filetype.ts').FileType;
var FileUtil = require('./fileutil.ts').FileUtil;

interface FileTypeMap {
    [key: string]: string[]
}

class FileTypes {
    private static fileTypeMap: FileTypeMap = FileTypes.getFileTypeMap();

    private static getFileTypeMap(): FileTypeMap {
        //TODO: move to config file
        let fileTypeMap: FileTypeMap = {};

        const DomJS = require("dom-js").DomJS;
        const domjs = new DomJS();

        const xml: string = fs.readFileSync(FileUtil.expandPath(config.FILETYPESPATH)).toString();
        domjs.parse(xml, function(err: Error, dom) {
            if (err) {
                throw err;
            }
            dom.children.forEach(child => {
                if (child.name && child.name === 'filetype') {
                    let name: string = child.attributes.name;
                    child.children.forEach(filetypeChild => {
                        if (filetypeChild.name && filetypeChild.name === 'extensions') {
                            let extensions: string = filetypeChild.children[0].text;
                            fileTypeMap[name] = common.setFromArray(extensions.split(/\s+/));
                        }
                    });
                }
            });
            fileTypeMap['text'] = [].concat(fileTypeMap['text'], fileTypeMap['code'], fileTypeMap['xml']);
            fileTypeMap['searchable'] = [].concat(fileTypeMap['text'], fileTypeMap['binary'],
                fileTypeMap['archive']);
        });
        return fileTypeMap;
    }

    public static fromName(name: string): FileType {
        if (name.toUpperCase() === 'TEXT')
            return FileType.Text;
        if (name.toUpperCase() === 'BINARY')
            return FileType.Binary;
        if (name.toUpperCase() === 'ARCHIVE')
            return FileType.Archive;
        return FileType.Unknown;
    }

    public static getFileType(filename: string): FileType {
        if (FileTypes.isTextFile(filename))
            return FileType.Text;
        if (FileTypes.isBinaryFile(filename))
            return FileType.Binary;
        if (FileTypes.isArchiveFile(filename))
            return FileType.Archive;
        return FileType.Unknown;
    }

    public static getFileTypeAsync(filename: string, cb): void {
        if (FileTypes.isTextFile(filename))
            return cb(FileType.Text);
        if (FileTypes.isBinaryFile(filename))
            return cb(FileType.Binary);
        if (FileTypes.isArchiveFile(filename))
            return cb(FileType.Archive);
        cb(FileType.Unknown);
    }

    public static isArchiveFile(filename: string): boolean {
        let ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['archive'].indexOf(ext) > -1;
    }

    public static isBinaryFile(filename: string): boolean {
        let ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['binary'].indexOf(ext) > -1;
    }

    public static isSearchableFile(filename: string): boolean {
        let ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['searchable'].indexOf(ext) > -1;
    }

    public static isTextFile(filename: string): boolean {
        let ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['text'].indexOf(ext) > -1;
    }

    public static isUnknownFile(filename: string): boolean {
        let ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['searchable'].indexOf(ext) === -1;
    }
}

exports.FileTypes = FileTypes;
