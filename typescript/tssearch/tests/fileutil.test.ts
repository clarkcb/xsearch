/*
 * fileutil.test.js
 *
 * Some tests of fileutil.js
 */

import {FileUtil} from '../src/fileutil';

describe('testing fileutil', () => {
    /***************************************************************************
     * getExtension tests
     **************************************************************************/
    it('testGetTxtExtension', () => {
        const file = "filename.txt";
        expect(FileUtil.getExtension(file)).toEqual("txt");
    });

    it('testGetMissingExtension', () => {
        const file = "filename.";
        expect(FileUtil.getExtension(file)).toEqual("");
    });

    it('testGetNoExtension', () => {
        const file = "filename";
        expect(FileUtil.getExtension(file)).toEqual("");
    });

    it('testGetHiddenTxtExtension', () => {
        const file = ".filename.txt";
        expect(FileUtil.getExtension(file)).toEqual("txt");
    });

    it('testGetHiddenMissingExtension', () => {
        const file = ".filename.";
        expect(FileUtil.getExtension(file)).toEqual("");
    });

    it('testGetHiddenNoExtension', () => {
        const file = ".filename";
        expect(FileUtil.getExtension(file)).toEqual("");
    });

    /***************************************************************************
     * getRelativePath tests
     **************************************************************************/
    it('testGetRelativePath', () => {
        const filepath = "/Users/cary/filename.txt";
        expect(FileUtil.getRelativePath(filepath, '.')).toEqual("./filename.txt");
    });

    /***************************************************************************
     * isDotDir tests
     **************************************************************************/
    it('testIsDotDirSingleDot', () => {
        const filepath = ".";
        expect(FileUtil.isDotDir(filepath)).toBeTruthy();
    });

    it('testIsDotDirSingleDotSlash', () => {
        const filepath = "./";
        expect(FileUtil.isDotDir(filepath)).toBeTruthy();
    });

    it('testIsDotDirDoubleDot', () => {
        const filepath = "..";
        expect(FileUtil.isDotDir(filepath)).toBeTruthy();
    });

    it('testIsDotDirDoubleDotSlash', () => {
        const filepath = "../";
        expect(FileUtil.isDotDir(filepath)).toBeTruthy();
    });

    it('testIsDotDirNotDotDir', () => {
        const filepath = "~/path";
        expect(FileUtil.isDotDir(filepath)).toBeFalsy();
    });

    it('testIsDotDirPathWithDot', () => {
        const filepath = "./path";
        expect(FileUtil.isDotDir(filepath)).toBeFalsy();
    });

    it('testIsDotDirHiddenFile', () => {
        const filepath = ".gitignore";
        expect(FileUtil.isDotDir(filepath)).toBeFalsy();
    });

    /***************************************************************************
     * isHidden tests
     **************************************************************************/
    it('testIsHiddenSingleDot', () => {
        const filepath = ".";
        expect(FileUtil.isHidden(filepath)).toBeFalsy();
    });

    it('testIsHiddenDoubleDot', () => {
        const filepath = "..";
        expect(FileUtil.isHidden(filepath)).toBeFalsy();
    });

    it('testIsHiddenHiddenFile', () => {
        const filepath = ".gitignore";
        expect(FileUtil.isHidden(filepath)).toBeTruthy();
    });

    it('testIsHiddenNotHiddenFile', () => {
        const filepath = "file.txt";
        expect(FileUtil.isHidden(filepath)).toBeFalsy();
    });

    /***************************************************************************
     * expandPath tests
     **************************************************************************/
    it('testExpandPathPathWithTilde', () => {
        const filepath = "~/filename.txt";
        const expected: string = process.env.HOME + "/filename.txt";
        expect(FileUtil.expandPath(filepath)).toEqual(expected);
    });

    it('testExpandPathPathNoTilde', () => {
        const filepath = "./filename.txt";
        expect(FileUtil.expandPath(filepath)).toEqual(filepath);
    });
});
