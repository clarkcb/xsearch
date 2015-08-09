/// <reference path="../typings/node/node.d.ts"/>
/// <reference path="filetype.ts"/>
/*
 * searchfile.js
 *
 * encapsulates a file to be searched
 */

var path = require('path');
//var FileType = require('./filetype.js').FileType;

class SearchFile {
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

    public toString(): string {
        var s: string = '';
        if (this.containers.length > 0) {
            s = this.containers.join(this.containerSeparator) + this.containerSeparator;
        }
        s += path.join(this.pathname, this.filename);
        return s;
    }
}

exports.SearchFile = SearchFile;