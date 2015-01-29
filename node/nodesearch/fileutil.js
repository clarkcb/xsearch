/*
 * fileutil.js
 *
 * file-related utility functions
 */

var path = require('path');

function FileUtil() {}

FileUtil.getExtension = function (filepath) {
    var f = path.basename(filepath);
    var idx = f.lastIndexOf('.');
    if (idx > 0 && idx < f.length-1) {
        return f.substring(idx+1);
    } else {
        return '';
    }
};

FileUtil.expandPath = function (filepath) {
    var idx = filepath.indexOf('~');
    if (idx === 0) {
        return process.env.HOME + filepath.substring(1);
    } else {
        return filepath;
    }
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
