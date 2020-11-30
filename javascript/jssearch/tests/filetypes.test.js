/*
 * filetypes_test.js
 *
 * Some tests of filetypes.js
 */

const {FileType} = require('../src/filetype');
const {FileTypes} = require('../src/filetypes');

describe('testing filetypes', () => {
    it('testFileTypesArchiveFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'archive.zip';
        const res = fileTypes.isArchiveFile(filename);
        expect(res).toBeTruthy();
        const type = fileTypes.getFileType(filename);
        expect(type).toBe(FileType.ARCHIVE);

        fileTypes.getFileTypeAsync(filename, (err, type) => {
            expect(err).toBeNull();
            expect(type).toBe(FileType.ARCHIVE);
        });

        const invalid_filename = 200;
        fileTypes.getFileTypeAsync(invalid_filename, (err, t) => {
            // console.log('err: ' + err);
            expect(err).toBeDefined();
            // console.log('type: ' + t);
            expect(t).toBeUndefined();
        });
    });

    it('testFileTypesBinaryFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'binary.exe';
        const res = fileTypes.isBinaryFile(filename);
        expect(res).toBeTruthy();
        const type = fileTypes.getFileType(filename);
        expect(type).toBe(FileType.BINARY);
    });

    it('testFileTypesCodeFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'code.js';
        const res = fileTypes.isCodeFile(filename);
        expect(res).toBeTruthy();
        const type = fileTypes.getFileType(filename);
        expect(type).toBe(FileType.CODE);
    });

    it('testFileTypesTextFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'text.txt';
        const res = fileTypes.isTextFile(filename);
        expect(res).toBeTruthy();
        const type = fileTypes.getFileType(filename);
        expect(type).toBe(FileType.TEXT);
    });

    it('testFileTypesXmlFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'markup.xml';
        const res = fileTypes.isXmlFile(filename);
        expect(res).toBeTruthy();
        const type = fileTypes.getFileType(filename);
        expect(type).toBe(FileType.XML);
    });

    it('testFileTypesSearchableFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'README.md';
        const res = fileTypes.isSearchableFile(filename);
        expect(res).toBeTruthy();
        const type = fileTypes.getFileType(filename);
        expect(type).toBe(FileType.TEXT);
    });

    it('testFileTypesUnknownFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'unknown.xyz';
        const res = fileTypes.isUnknownFile(filename);
        expect(res).toBeTruthy();
        const type = fileTypes.getFileType(filename);
        expect(type).toBe(FileType.UNKNOWN);
    });
});
