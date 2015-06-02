/*
 * searchfile.js
 *
 * encapsulates a file to be searched
 */

var path = require('path');

function SearchFile(path, filename, filetype) {
    var self = this;
    self.containerSeparator = '!';
    self.containers = [];
    self.path = path;
    self.filename = filename;
    self.filetype = filetype;

    self.toString = function () {
        var s = '';
        if (self.containers.length > {
            s = self.containers.join(self.containerSeparator) + self.containerSeparator;
        }
        s += path.join(self.path, self.filename);

        return s;
    };
}

exports.SearchFile = SearchFile;
