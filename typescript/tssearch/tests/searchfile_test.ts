/*
 * searchfile_test.js
 *
 * Some nodeunit tests of searchfile.js
 */

import {FileType} from '../src/filetype';
import {SearchFile} from '../src/searchfile';

exports.testSearchFileAbsPath = function(test) {
    const pathname = '/Users/cary/src/xsearch/javascript/jssearch/src';
    const filename = 'searchfile.js';
    const searchfile = new SearchFile(pathname, filename, FileType.Code);
    const expected = '/Users/cary/src/xsearch/javascript/jssearch/src/searchfile.js';
    test.ok(searchfile.relativePath() === expected, "searchfile path doesn't match expected");
    test.done();
};

exports.testSearchFileRelPath1 = function(test) {
    const pathname = '.';
    const filename = 'searchfile.js';
    const searchfile = new SearchFile(pathname, filename, FileType.Code);
    const expected = './searchfile.js';
    test.ok(searchfile.relativePath() === expected, "searchfile path doesn't match expected");
    test.done();
};

exports.testSearchFileRelPath2 = function(test) {
    const pathname = './';
    const filename = 'searchfile.js';
    const searchfile = new SearchFile(pathname, filename, FileType.Code);
    const expected = './searchfile.js';
    test.ok(searchfile.relativePath() === expected, "searchfile path doesn't match expected");
    test.done();
};

exports.testSearchFileRelPath3 = function(test) {
    const pathname = '..';
    const filename = 'searchfile.js';
    const searchfile = new SearchFile(pathname, filename, FileType.Code);
    const expected = '../searchfile.js';
    test.ok(searchfile.relativePath() === expected, "searchfile path doesn't match expected");
    test.done();
};
