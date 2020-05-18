/*
 * fileutil.js
 *
 * file-related utility functions
 */

"use strict";

const fs = require('fs');
const path = require('path');

const common = require('./common');

const { promisify } = require('util');
const readFileAsync = promisify(fs.readFile);

exports.expandPath = filepath => {
    let idx = filepath.indexOf('~');
    return idx === 0 ? process.env.HOME + filepath.substring(1) : filepath;
};

exports.expandPathAsync = (filepath, cb) => {
    let idx = filepath.indexOf('~');
    return cb(null, idx === 0 ? process.env.HOME + filepath.substring(1) : filepath);
};

exports.getExtension = filepath => {
    try {
        let f = path.basename(filepath);
        let idx = f.lastIndexOf('.');
        if (idx > 0 && idx < f.length-1) {
            return f.substring(idx+1);
        } else {
            return '';
        }
    } catch (err) {
        throw err;
    }
};

exports.getExtensionAsync = (filepath, cb) => {
    try {
        let f = path.basename(filepath);
        let idx = f.lastIndexOf('.');
        if (idx > 0 && idx < f.length-1) {
            return cb(null, f.substring(idx+1));
        } else {
            return cb(null, '');
        }
    } catch (err) {
        return cb(err);
    }
};

exports.getFileContents = (filepath, encoding) => {
    return fs.readFileSync(filepath, encoding).toString();
};

exports.getFileContentsAsync = async (filepath, encoding) => {
    try {
        let data = await readFileAsync(filepath, encoding);
        return data;
    } catch (err) {
        throw err;
    }
};

exports.getFileContentsCallback = (filepath, encoding, cb) => {
    fs.readFile(filepath, encoding, (err, data) => {
        if (err) {
            common.log("An error occurred trying to read file: " + filepath);
            cb(err);
        }
        cb(null, data.toString());
    });
};

exports.getFileLines = (filepath, encoding) => {
    return exports.getFileContents(filepath, encoding).split(/\r?\n/);
};

exports.getFileLinesAsync = (filepath, encoding, cb) => {
    exports.getFileContentsCallback(filepath, encoding, (err, contents) => {
        if (err) {
            cb(err);
        }
        cb(null, contents.split(/\r?\n/))
    });
};

exports.getRelativePath = (filepath, startpath) => {
    if (startpath === '.' && filepath.startsWith(process.env.HOME)) {
        return '.' + filepath.substring(process.env.HOME.length);
    }
};

exports.isDotDir = filepath => {
    return ['.', '..', './', '../'].indexOf(filepath) > -1;
};

exports.isHidden = filepath => {
    let f = path.basename(filepath);
    return f.length > 1 && f.charAt(0) === '.' && !exports.isDotDir(f);
};
