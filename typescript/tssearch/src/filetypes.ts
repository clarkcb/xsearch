/*
 * filetypes.ts
 *
 * identifies file types (archive, binary, text, unknown)
 */

"use strict";

import * as config from './config';
import * as common from './common';
import {FileType} from './FileType';
import {FileUtil} from './fileutil';

interface FileTypeMap {
    [key: string]: string[]
}

export class FileTypes {
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
        let uname: string = name.toUpperCase();
        if (uname === 'ARCHIVE')
            return FileType.Archive;
        if (uname === 'BINARY')
            return FileType.Binary;
        if (uname === 'CODE')
            return FileType.Code;
        if (uname === 'TEXT')
            return FileType.Text;
        if (uname === 'XML')
            return FileType.Xml;
        return FileType.Unknown;
    }

    public static toName(fileType: FileType): string {
        if (fileType === FileType.Archive)
            return 'ARCHIVE';
        if (fileType === FileType.Binary)
            return 'BINARY';
        if (fileType === FileType.Code)
            return 'CODE';
        if (fileType === FileType.Text)
            return 'TEXT';
        if (fileType === FileType.Xml)
            return 'XML';
        return 'UNKNOWN';
    }

    public static getFileType(filename: string): FileType {
        if (FileTypes.isCodeFile(filename))
            return FileType.Code;
        if (FileTypes.isXmlFile(filename))
            return FileType.Xml;
        if (FileTypes.isTextFile(filename))
            return FileType.Text;
        if (FileTypes.isBinaryFile(filename))
            return FileType.Binary;
        if (FileTypes.isArchiveFile(filename))
            return FileType.Archive;
        return FileType.Unknown;
    }

    public static getFileTypeAsync(filename: string, cb: (ft: FileType) => void): void {
        if (FileTypes.isCodeFile(filename))
            return cb(FileType.Code);
        if (FileTypes.isXmlFile(filename))
            return cb(FileType.Xml);
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

    public static isCodeFile(filename: string): boolean {
        let ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['code'].indexOf(ext) > -1;
    }

    public static isSearchableFile(filename: string): boolean {
        let ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['searchable'].indexOf(ext) > -1;
    }

    public static isTextFile(filename: string): boolean {
        let ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['text'].indexOf(ext) > -1;
    }

    public static isXmlFile(filename: string): boolean {
        let ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['xml'].indexOf(ext) > -1;
    }

    public static isUnknownFile(filename: string): boolean {
        let ext: string = FileUtil.getExtension(filename);
        return FileTypes.fileTypeMap['searchable'].indexOf(ext) === -1;
    }
}
