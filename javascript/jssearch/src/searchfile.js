/*
 * searchfile.js
 *
 * encapsulates a file to be searched
 */

const path = require('path');

function SearchFile(pathname, filename, filetype) {
    "use strict";
    let self = this;
    self.containerSeparator = '!';
    self.containers = [];
    self.pathname = pathname;
    self.filename = filename;
    self.filetype = filetype;

    self.relativePath = () => {
        if (self.pathname === '.' || self.pathname === './') return './' + self.filename;
        return path.join(self.pathname, self.filename);
    };

    self.toString = () => {
        let s = '';
        if (self.containers.length > 0) {
            s = self.containers.join(self.containerSeparator) + self.containerSeparator;
        }
        s += self.relativePath();

        return s;
    };
}

exports.SearchFile = SearchFile;
