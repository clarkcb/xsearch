/// <reference path="../typings/node/node.d.ts"/>
/// <reference path="filetype.ts"/>
/// <reference path="fileutil.ts"/>
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
        var fileTypeMap: FileTypeMap = {};

        var DomJS = require("dom-js").DomJS;
        var domjs = new DomJS();

        var xml: string = fs.readFileSync(FileUtil.expandPath(config.FILETYPESPATH)).toString();
        domjs.parse(xml, function(err: Error, dom) {
            if (err) {
                throw err;
            }
            for (var i: number = 0; i < dom.children.length; i++) {
                var child = dom.children[i];
                if (child.name && child.name === 'filetype') {
                    var name: string = child.attributes.name;
                    for (var j: number = 0; j < child.children.length; j++) {
                        var filetypeChild = child.children[j];
                        if (filetypeChild.name && filetypeChild.name === 'extensions') {
                            var extensions: string = filetypeChild.children[0].text;
                            fileTypeMap[name] = common.setFromArray(extensions.split(/\s+/));
                        }
                    }
                }
            }
            fileTypeMap['text'] = [].concat(fileTypeMap['text'], fileTypeMap['code'], fileTypeMap['xml']);
            fileTypeMap['searchable'] = [].concat(fileTypeMap['text'], fileTypeMap['binary'],
                fileTypeMap['archive']);
        });
        return fileTypeMap;
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
        var ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['archive'].indexOf(ext) > -1;
    }

    public static isBinaryFile(filename: string): boolean {
        var ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['binary'].indexOf(ext) > -1;
    }

    public static isSearchableFile(filename: string): boolean {
        var ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['searchable'].indexOf(ext) > -1;
    }

    public static isTextFile(filename: string): boolean {
        var ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['text'].indexOf(ext) > -1;
    }

    public static isUnknownFile(filename: string): boolean {
        var ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['searchable'].indexOf(ext) === -1;
    }
}

exports.FileTypes = FileTypes;
