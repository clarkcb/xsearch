/*
 * filetypes.test.js
 *
 * Some tests of filetypes.js
 */

import {FileType} from '../src/filetype';
import {FileTypes} from '../src/filetypes';

describe('testing filetypes', () => {
    it('testFileTypesArchiveFile', () => {
        const filename = 'archive.zip';
        const res: boolean = FileTypes.isArchiveFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Archive);
    });

    it('testFileTypesBinaryFile', () => {
        const filename = 'binary.exe';
        const res: boolean = FileTypes.isBinaryFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Binary);
    });

    it('testFileTypesCodeFile', () => {
        const filename = 'code.js';
        const res: boolean = FileTypes.isCodeFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Code);
    });

    it('testFileTypesTextFile', () => {
        const filename = 'text.txt';
        const res: boolean = FileTypes.isTextFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Text);
    });

    it('testFileTypesXmlFile', () => {
        const filename = 'markup.xml';
        const res: boolean = FileTypes.isXmlFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Xml);
    });

    it('testFileTypesUnknownFile', () => {
        const filename = 'unknown.xyz';
        const res: boolean = FileTypes.isUnknownFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Unknown);
    });
});
