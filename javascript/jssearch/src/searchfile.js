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

    self.toString = function () {
        let s = '';
        if (self.containers.length > 0) {
            s = self.containers.join(self.containerSeparator) + self.containerSeparator;
        }
        s += path.join(self.pathname, self.filename);

        return s;
    };
}

exports.SearchFile = SearchFile;
