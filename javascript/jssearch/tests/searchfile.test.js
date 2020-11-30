/*
 * searchresult_test.js
 *
 * Some tests of searchresult.js
 */

const {FileType} = require('../src/filetype');
const SearchFile = require('../src/searchfile').SearchFile;

describe('testing searchfile', () => {
    it('testSearchFileAbsPath', () => {
        const pathname = '/Users/cary/src/xsearch/javascript/jssearch/src';
        const filename = 'searchfile.js';
        const searchfile = new SearchFile(pathname, filename, FileType.CODE);
        const expected = '/Users/cary/src/xsearch/javascript/jssearch/src/searchfile.js';
        expect(searchfile.relativePath()).toEqual(expected);
    });

    it('testSearchFileRelPath1', () => {
        const pathname = '.';
        const filename = 'searchfile.js';
        const searchfile = new SearchFile(pathname, filename, FileType.CODE);
        const expected = './searchfile.js';
        expect(searchfile.relativePath()).toEqual(expected);
    });

    it('testSearchFileRelPath2', () => {
        const pathname = './';
        const filename = 'searchfile.js';
        const searchfile = new SearchFile(pathname, filename, FileType.CODE);
        const expected = './searchfile.js';
        expect(searchfile.relativePath()).toEqual(expected);
    });

    it('testSearchFileRelPath3', () => {
        const pathname = '..';
        const filename = 'searchfile.js';
        const searchfile = new SearchFile(pathname, filename, FileType.CODE);
        const expected = '../searchfile.js';
        expect(searchfile.relativePath()).toEqual(expected);
    });
});
