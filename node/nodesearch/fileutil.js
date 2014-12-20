/*
 * nodesearch.js
 *
 * file search utility written in node.js
 */

var fs = require('fs');

function FileUtil() {
    var that = this;

    var setFromArray = function (arr) {
        var hash = {};
        var set = [];
        for (var a in arr) {
            hash[arr[a]] = 1;
        }
        for (var h in hash) {
            set.push(h);
        }
        return set;
    }

    var fileTypeMap = (function () {
        //TODO: move to config file
        var fileTypePath = "/Users/cary/src/git/xsearch/shared/filetypes.xml";
        var fileTypeMap = {};

        var DomJS = require("dom-js").DomJS;
        var domjs = new DomJS();

        var xml = fs.readFileSync(fileTypePath).toString();
        domjs.parse(xml, function(err, dom) {
            if (err) {
                throw err;
            }
            for (var i in dom.children) {
                var child = dom.children[i];
                if (child.name && child.name === 'filetype') {
                    var name = child.attributes.name;
                    for (var j in child.children) {
                        var filetypeChild = child.children[j];
                        if (filetypeChild.name && filetypeChild.name === 'extensions') {
                            var extensions = filetypeChild.children[0].text;
                            fileTypeMap[name] = setFromArray(extensions.split(/\s+/));
                        }
                    }
                }
            }
            fileTypeMap['text'] = [].concat(fileTypeMap['text'], fileTypeMap['code'], fileTypeMap['xml']);
            fileTypeMap['searchable'] = [].concat(fileTypeMap['text'], fileTypeMap['binary'],
                fileTypeMap['compressed']);
        });
        return fileTypeMap;
    })();

    this.getExtension = function (filename) {
        var idx = filename.lastIndexOf('.');
        if (idx > 0 && idx < filename.length-1) {
            return filename.substring(idx+1);
        } else {
            return '';
        }
    };
    this.isArchiveFile = function (filename) {
        var ext = that.getExtension(filename);
        return fileTypeMap['archive'].indexOf(ext) > -1;
    };
    this.isBinaryFile = function (filename) {
        var ext = that.getExtension(filename);
        return fileTypeMap['binary'].indexOf(ext) > -1;
    };
    this.isSearchableFile = function (filename) {
        var ext = that.getExtension(filename);
        return fileTypeMap['searchable'].indexOf(ext) > -1;
    };
    this.isTextFile = function (filename) {
        var ext = that.getExtension(filename);
        return fileTypeMap['text'].indexOf(ext) > -1;
    };
}

exports.FileUtil = FileUtil;