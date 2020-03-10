/*
 * searchresult_test.js
 *
 * Some nodeunit tests of searchresult.js
 */

const FileType = require('../src/filetype').FileType;
const SearchFile = require('../src/searchfile').SearchFile;

exports.testSearchFileAbsPath = (test) => {
    const pathname = '/Users/cary/src/xsearch/javascript/jssearch/src';
    const filename = 'searchfile.js';
    const searchfile = new SearchFile(pathname, filename, FileType.CODE);
    const expected = '/Users/cary/src/xsearch/javascript/jssearch/src/searchfile.js';
    test.ok(searchfile.relativePath() === expected, "searchfile path doesn't match expected");
    test.done();
};

exports.testSearchFileRelPath1 = (test) => {
    const pathname = '.';
    const filename = 'searchfile.js';
    const searchfile = new SearchFile(pathname, filename, FileType.CODE);
    const expected = './searchfile.js';
    test.ok(searchfile.relativePath() === expected, "searchfile path doesn't match expected");
    test.done();
};

exports.testSearchFileRelPath2 = (test) => {
    const pathname = './';
    const filename = 'searchfile.js';
    const searchfile = new SearchFile(pathname, filename, FileType.CODE);
    const expected = './searchfile.js';
    test.ok(searchfile.relativePath() === expected, "searchfile path doesn't match expected");
    test.done();
};

exports.testSearchFileRelPath3 = (test) => {
    const pathname = '..';
    const filename = 'searchfile.js';
    const searchfile = new SearchFile(pathname, filename, FileType.CODE);
    const expected = '../searchfile.js';
    test.ok(searchfile.relativePath() === expected, "searchfile path doesn't match expected");
    test.done();
};
