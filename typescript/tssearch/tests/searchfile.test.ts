/*
 * searchfile.test.js
 *
 * Some tests of searchfile.js
 */

import {FileType} from '../src/filetype';
import {SearchFile} from '../src/searchfile';

describe('testing searchfile', () => {
    it('testSearchFileAbsPath', () => {
        const pathname = '/Users/cary/src/xsearch/javascript/jssearch/src';
        const filename = 'searchfile.js';
        const searchfile = new SearchFile(pathname, filename, FileType.Code);
        const expected = '/Users/cary/src/xsearch/javascript/jssearch/src/searchfile.js';
        expect(searchfile.relativePath()).toEqual(expected);
    });

    it('testSearchFileTildePath', () => {
        const pathname = '~/src/xsearch/javascript/jssearch/src';
        const filename = 'searchfile.js';
        const searchfile = new SearchFile(pathname, filename, FileType.Code);
        const expected = '~/src/xsearch/javascript/jssearch/src/searchfile.js';
        expect(searchfile.relativePath()).toEqual(expected);
    });

    it('testSearchFileRelPath1', () => {
        const pathname = '.';
        const filename = 'searchfile.js';
        const searchfile = new SearchFile(pathname, filename, FileType.Code);
        const expected = './searchfile.js';
        expect(searchfile.relativePath()).toEqual(expected);
    });

    it('testSearchFileRelPath2', () => {
        const pathname = './';
        const filename = 'searchfile.js';
        const searchfile = new SearchFile(pathname, filename, FileType.Code);
        const expected = './searchfile.js';
        expect(searchfile.relativePath()).toEqual(expected);
    });

    it('testSearchFileRelPath3', () => {
        const pathname = '..';
        const filename = 'searchfile.js';
        const searchfile = new SearchFile(pathname, filename, FileType.Code);
        const expected = '../searchfile.js';
        expect(searchfile.relativePath()).toEqual(expected);
    });
});
