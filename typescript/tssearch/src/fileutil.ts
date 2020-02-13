/*
 * fileutil.js
 *
 * file-related utility functions
 */

"use strict";

const fs = require('fs');
const path = require('path');

import * as config from './config';

export class FileUtil {

    public static expandPath(filepath: string): string {
        let idx: number = filepath.indexOf('~');
        return idx === 0 ? process.env.HOME + filepath.substring(1) : filepath;
    }

    public static getExtension(filepath: string): string {
        let f: string = path.basename(filepath);
        let idx: number = f.lastIndexOf('.');
        if (idx > 0 && idx < f.length - 1) {
            return f.substring(idx + 1);
        }
        return '';
    }

    public static getFileContents(filepath: string): string {
        return fs.readFileSync(filepath).toString();
    }

    public static getFileContentsAsync(filepath: string, cb): void {
        cb(fs.readFileSync(filepath).toString());
    }

    public static getFileLines(filepath: string): string[] {
        return FileUtil.getFileContents(filepath).split(/\r?\n/);
    }

    public static getFileLinesAsync(filepath: string, cb): void {
        cb(FileUtil.getFileContents(filepath).split(/\r?\n/));
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
        let f: string = path.basename(filepath);
        return f.length > 1 && f.charAt(0) == '.' && !FileUtil.isDotDir(f);
    }
}
