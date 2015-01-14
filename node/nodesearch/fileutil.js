/*
 * fileutil.js
 *
 * file-related utility functions
 */


function FileUtil() {}

FileUtil.getExtension = function (filename) {
    var idx = filename.lastIndexOf('.');
    if (idx > 0 && idx < filename.length-1) {
        return filename.substring(idx+1);
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

exports.FileUtil = FileUtil;
