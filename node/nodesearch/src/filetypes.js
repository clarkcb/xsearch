/*
 * filetypes.js
 *
 * identifies file types (archive, binary, text, unknown)
 */

var fs = require('fs');

var common = require('./common.js');
var config = require('./config.js');
var FileType = require('./filetype.js').FileType;
var FileUtil = require('./fileutil.js').FileUtil;

function FileTypes() {
    var self = this;

    var fileTypeMap = (function () {
        //TODO: move to config file
        var fileTypeMap = {};

        var DomJS = require("dom-js").DomJS;
        var domjs = new DomJS();

        var xml = fs.readFileSync(FileUtil.expandPath(config.FILETYPESPATH)).toString();
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
                            fileTypeMap[name] = common.setFromArray(extensions.split(/\s+/));
                        }
                    }
                }
            }
            fileTypeMap.text = [].concat(fileTypeMap.text, fileTypeMap.code, fileTypeMap.xml);
            fileTypeMap.searchable = [].concat(fileTypeMap.text, fileTypeMap.binary,
                fileTypeMap.archive);
        });
        return fileTypeMap;
    })();

    this.getFileType = function (filename) {
        if (self.isTextFile(filename))
            return FileType.TEXT;
        if (self.isBinaryFile(filename))
            return FileType.BINARY;
        if (self.isArchiveFile(filename))
            return FileType.ARCHIVE;
        return FileType.UNKNOWN;
    };

    this.isArchiveFile = function (filename) {
        var ext = FileUtil.getExtension(filename);
        return fileTypeMap.archive.indexOf(ext) > -1;
    };

    this.isBinaryFile = function (filename) {
        var ext = FileUtil.getExtension(filename);
        return fileTypeMap.binary.indexOf(ext) > -1;
    };

    this.isSearchableFile = function (filename) {
        var ext = FileUtil.getExtension(filename);
        return fileTypeMap.searchable.indexOf(ext) > -1;
    };

    this.isTextFile = function (filename) {
        var ext = FileUtil.getExtension(filename);
        return fileTypeMap.text.indexOf(ext) > -1;
    };

    this.isUnknownFile = function (filename) {
        var ext = FileUtil.getExtension(filename);
        return fileTypeMap.searchable.indexOf(ext) === -1;
    };
}

exports.FileTypes = FileTypes;