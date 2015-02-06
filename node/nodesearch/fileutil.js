/*
 * fileutil.js
 *
 * file-related utility functions
 */

var fs = require('fs');
var path = require('path');

function FileUtil() {}

FileUtil.expandPath = function (filepath) {
    var idx = filepath.indexOf('~');
    if (idx === 0) {
        return process.env.HOME + filepath.substring(1);
    } else {
        return filepath;
    }
};

FileUtil.getExtension = function (filepath) {
    var f = path.basename(filepath);
    var idx = f.lastIndexOf('.');
    if (idx > 0 && idx < f.length-1) {
        return f.substring(idx+1);
    } else {
        return '';
    }
};

FileUtil.getFileContents = function (filepath) {
    return fs.readFileSync(filepath).toString();
};

FileUtil.getFileLines = function (filepath) {
    return FileUtil.getFileContents(filepath).split(/\r?\n/);
};

FileUtil.isDotDir = function (filepath) {
    return ['.', '..'].indexOf(filepath) > -1;
};

FileUtil.isHidden = function (filepath) {
    var f = path.basename(filepath);
    if (f.length > 1 && f.charAt(0) == '.' && !FileUtil.isDotDir(f)) {
        return true;
    } else {
        return false;
    }
};

exports.FileUtil = FileUtil;
