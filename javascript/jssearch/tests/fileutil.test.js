/*
 * fileutil_test.js
 *
 * Some tests of fileutil.js
 */

const FileUtil = require('../src/fileutil');

describe('testing fileutil', () => {
    /***************************************************************************
     * getExtension tests
     **************************************************************************/
    it('testGetTxtExtension', () => {
        const file = 'filename.txt';
        expect(FileUtil.getExtension(file)).toEqual('txt');
    });

    it('testGetTxtExtensionAsync', () => {
        const file = 'filename.txt';
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeNull();
            expect(ext).toBe('txt');
        });
    });

    it('testGetMissingExtension', () => {
        const file = 'filename.';
        expect(FileUtil.getExtension(file)).toEqual('');
    });

    it('testGetMissingExtensionAsync', () => {
        const file = 'filename.';
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeNull();
            expect(ext).toBe('');
        });
    });

    it('testGetNoExtension', () => {
        const file = 'filename';
        expect(FileUtil.getExtension(file)).toEqual('');
    });

    it('testGetNoExtensionAsync', () => {
        const file = 'filename';
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeNull();
            expect(ext).toEqual('');
        });
    });

    it('testGetHiddenTxtExtension', () => {
        const file = '.filename.txt';
        expect(FileUtil.getExtension(file)).toEqual('txt');
    });

    it('testGetHiddenTxtExtensionAsync', () => {
        const file = '.filename.txt';
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeNull();
            expect(ext).toBe('txt');
        });
    });

    it('testGetHiddenMissingExtension', () => {
        const file = 'filename.';
        expect(FileUtil.getExtension(file)).toEqual('');
    });

    it('testGetHiddenMissingExtensionAsync', () => {
        const file = 'filename.';
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeNull();
            expect(ext).toBe('');
        });
    });

    it('testGetHiddenNoExtension', () => {
        const file = 'filename';
        expect(FileUtil.getExtension(file)).toEqual('');
    });

    it('testGetHiddenNoExtensionAsync', () => {
        const file = 'filename';
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeNull();
            expect(ext).toBe('');
        });
    });

    it('testGetExtensionNonString', () => {
        const file = 200;
        try {
            const ext = FileUtil.getExtension(file);
        } catch (err) {
            expect(err).toBeDefined();
        }
    });

    it('testGetExtensionNonStringAsync', () => {
        const file = 200;
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeDefined();
            expect(ext).toBeUndefined();
        });
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
        const filename = ".";
        expect(FileUtil.isDotDir(filename)).toBeTruthy();
    });

    it('testIsDotDirDoubleDot', () => {
        const filename = "..";
        expect(FileUtil.isDotDir(filename)).toBeTruthy();
    });

    it('testIsDotDirNotDotDir', () => {
        const filename = "~/path";
        expect(FileUtil.isDotDir(filename)).toBeFalsy();
    });

    it('testIsDotDirPathWithDot', () => {
        const filename = "./path";
        expect(FileUtil.isDotDir(filename)).toBeFalsy();
    });

    it('testIsDotDirHiddenFile', () => {
        const filename = ".gitignore";
        expect(FileUtil.isDotDir(filename)).toBeFalsy();
    });

    /***************************************************************************
     * isHidden tests
     **************************************************************************/
    it('testIsHiddenSingleDot', () => {
        const filename = ".";
        expect(FileUtil.isHidden(filename)).toBeFalsy();
    });

    it('testIsHiddenDoubleDot', () => {
        const filename = "..";
        expect(FileUtil.isHidden(filename)).toBeFalsy();
    });

    it('testIsHiddenHiddenFile', () => {
        const filename = ".gitignore";
        expect(FileUtil.isHidden(filename)).toBeTruthy();
    });

    it('testIsHiddenNotHiddenFile', () => {
        const filename = "file.txt";
        expect(FileUtil.isHidden(filename)).toBeFalsy();
    });

    /***************************************************************************
     * expandPath tests
     **************************************************************************/
    it('testExpandPathPathWithTilde', () => {
        const filepath = "~/filename.txt";
        const expected = process.env.HOME + "/filename.txt";
        expect(FileUtil.expandPath(filepath) === expected).toBeTruthy();
    });

    it('testExpandPathPathNoTilde', () => {
        const filepath = "./filename.txt";
        expect(FileUtil.expandPath(filepath) === filepath).toBeTruthy();
    });
});
