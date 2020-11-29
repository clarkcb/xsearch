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
        if (filepath[0] === "~") {
            return config.HOME + filepath.substring(1);
        }
        return filepath;
    }

    public static getExtension(filepath: string): string {
        const f: string = path.basename(filepath);
        const idx: number = f.lastIndexOf('.');
        if (idx > 0 && idx < f.length - 1) {
            return f.substring(idx + 1);
        }
        return '';
    }

    public static getFileContents(filepath: string, encoding: string): string {
        return fs.readFileSync(filepath, encoding).toString();
    }

    public static getFileContentsAsync(filepath: string, cb: (contents: string) => void): void {
        cb(fs.readFileSync(filepath).toString());
    }

    public static getFileLines(filepath: string, encoding: string): string[] {
        return FileUtil.getFileContents(filepath, encoding).split(/\r?\n/);
    }

    public static getFileLinesAsync(filepath: string, encoding: string, cb: (lines: string[]) => void): void {
        cb(FileUtil.getFileContents(filepath, encoding).split(/\r?\n/));
    }

    public static getRelativePath(filepath: string, startpath: string): string {
        if (startpath === '.' && filepath.startsWith(config.HOME)) {
            return '.' + filepath.substring(config.HOME.length);
        }
        return filepath;
    }

    public static isDotDir(filepath: string): boolean {
        return ['.', './', '..', '../'].indexOf(filepath) > -1;
    }

    public static isHidden(filepath: string): boolean {
        const f: string = path.basename(filepath);
        return f.length > 1 && f.charAt(0) == '.' && !FileUtil.isDotDir(f);
    }
}
