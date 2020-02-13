/*
 * searchfile.js
 *
 * encapsulates a file to be searched
 */

"use strict";

import {FileType} from './filetype';

var path = require('path');

export class SearchFile {
    containerSeparator: string = '!';
    containers: string[] = [];
    pathname: string;
    filename: string;
    filetype: FileType;

    constructor(pathname: string, filename: string, filetype: FileType) {
        this.pathname = pathname;
        this.filename = filename;
        this.filetype = filetype;
    }

    public relativePath(): string {
        if (this.pathname === '.' || this.pathname === './') return './' + this.filename;
        return path.join(this.pathname, this.filename);
    };

    public toString(): string {
        let s: string = '';
        if (this.containers.length > 0) {
            s = this.containers.join(this.containerSeparator) + this.containerSeparator;
        }
        s += path.join(this.pathname, this.filename);
        return s;
    }
}
