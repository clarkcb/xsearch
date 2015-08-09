/// <reference path="../typings/node/node.d.ts"/>
/// <reference path="common.ts"/>
/*
 * fileutil.js
 *
 * file-related utility functions
 */

"use strict";

var fs = require('fs');
var path = require('path');

var common = require('./common.js');

class FileUtil {

    public static expandPath(filepath: string): string {
        var idx: number = filepath.indexOf('~');
        return idx === 0 ? process.env.HOME + filepath.substring(1) : filepath;
    }

    public static getExtension(filepath: string): string {
        var f: string = path.basename(filepath);
        var idx: number = f.lastIndexOf('.');
        if (idx > 0 && idx < f.length - 1) {
            return f.substring(idx + 1);
        }
        return '';
    }

    public static getFileContents(filepath: string): string {
        return fs.readFileSync(filepath).toString();
    }

    public static getFileLines(filepath: string): string[] {
        return FileUtil.getFileContents(filepath).split(/\r?\n/);
    }

    public static getRelativePath(filepath: string, startpath: string): string {
        if (startpath === '.' && filepath.startsWith(process.env.HOME)) {
            return '.' + filepath.substring(process.env.HOME.length);
        }
    }

    public static isDotDir(filepath: string): boolean {
        return ['.', '..'].indexOf(filepath) > -1;
    }

    public static isHidden(filepath: string): boolean {
        var f: string = path.basename(filepath);
        return f.length > 1 && f.charAt(0) == '.' && !FileUtil.isDotDir(f);
    }
}

exports.FileUtil = FileUtil;
