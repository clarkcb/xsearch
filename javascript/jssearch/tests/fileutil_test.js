/*
 * fileutil_test.js
 *
 * Some nodeunit tests of fileutil.js
 */

const FileUtil = require('../src/fileutil').FileUtil;

/***************************************************************************
 * getExtension tests
 **************************************************************************/
exports.testGetTxtExtension = (test) => {
    const file = 'filename.txt';
    test.ok(FileUtil.getExtension(file) === 'txt', 'ext === "txt"');
    test.done();
};

exports.testGetTxtExtensionAsync = (test) => {
    const file = 'filename.txt';
    FileUtil.getExtensionAsync(file, (err, ext) => {
        test.ok(err === null, 'err === null');
        test.ok(ext === 'txt', 'ext === "txt"');
        test.done();
    });
};

exports.testGetMissingExtension = (test) => {
    const file = 'filename.';
    test.ok(FileUtil.getExtension(file) === '', 'ext === ""');
    test.done();
};

exports.testGetMissingExtensionAsync = (test) => {
    const file = 'filename.';
    FileUtil.getExtensionAsync(file, (err, ext) => {
        test.ok(err === null, 'err === null');
        test.ok(ext === '', 'ext === ""');
        test.done();
    });
};

exports.testGetNoExtension = (test) => {
    const file = 'filename';
    test.ok(FileUtil.getExtension(file) === '', 'ext === ""');
    test.done();
};

exports.testGetNoExtensionAsync = (test) => {
    const file = 'filename';
    FileUtil.getExtensionAsync(file, (err, ext) => {
        test.ok(err === null, 'err === null');
        test.ok(ext === '', 'ext === ""');
        test.done();
    });
};

exports.testGetHiddenTxtExtension = (test) => {
    const file = '.filename.txt';
    test.ok(FileUtil.getExtension(file) === 'txt', 'ext === "txt"');
    test.done();
};

exports.testGetHiddenTxtExtensionAsync = (test) => {
    const file = '.filename.txt';
    FileUtil.getExtensionAsync(file, (err, ext) => {
        test.ok(err === null, 'err === null');
        test.ok(ext === 'txt', 'ext === "txt"');
        test.done();
    });
};

exports.testGetHiddenMissingExtension = (test) => {
    const file = 'filename.';
    test.ok(FileUtil.getExtension(file) === '', 'ext === ""');
    test.done();
};

exports.testGetHiddenMissingExtensionAsync = (test) => {
    const file = 'filename.';
    FileUtil.getExtensionAsync(file, (err, ext) => {
        test.ok(err === null, 'err === null');
        test.ok(ext === '', 'ext === ""');
        test.done();
    });
};

exports.testGetHiddenNoExtension = (test) => {
    const file = 'filename';
    test.ok(FileUtil.getExtension(file) === '', 'ext === ""');
    test.done();
};

exports.testGetHiddenNoExtensionAsync = (test) => {
    const file = 'filename';
    FileUtil.getExtensionAsync(file, (err, ext) => {
        test.ok(err === null, 'err === null');
        test.ok(ext === '', 'ext === ""');
        test.done();
    });
};

exports.testGetExtensionNonString = (test) => {
    const file = 200;
    try {
        const ext = FileUtil.getExtension(file);
    } catch (err) {
        test.ok(err !== null, 'err !== null');
    }
    test.done();
};

exports.testGetExtensionNonStringAsync = (test) => {
    const file = 200;
    FileUtil.getExtensionAsync(file, (err, ext) => {
        test.ok(err !== null, 'err !== null');
        test.ok(ext === undefined, 'ext == undefined');
        test.done();
    });
};

/***************************************************************************
 * getRelativePath tests
 **************************************************************************/
exports.testGetRelativePath = (test) => {
    const filepath = "/Users/cary/filename.txt";
    test.ok(FileUtil.getRelativePath(filepath, '.') === "./filename.txt", "relativepath === ./filenametxt");
    test.done();
};

/***************************************************************************
 * isDotDir tests
 **************************************************************************/
exports.testIsDotDirSingleDot = (test) => {
    const filename = ".";
    test.ok(FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == true");
    test.done();
};

exports.testIsDotDirDoubleDot = (test) => {
    const filename = "..";
    test.ok(FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == true");
    test.done();
};

exports.testIsDotDirNotDotDir = (test) => {
    const filename = "~/path";
    test.ok(!FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == false");
    test.done();
};

exports.testIsDotDirPathWithDot = (test) => {
    const filename = "./path";
    test.ok(!FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == false");
    test.done();
};

exports.testIsDotDirHiddenFile = (test) => {
    const filename = ".gitignore";
    test.ok(!FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == false");
    test.done();
};

/***************************************************************************
 * isHidden tests
 **************************************************************************/
exports.testIsHiddenSingleDot = (test) => {
    const filename = ".";
    test.ok(!FileUtil.isHidden(filename));
    test.done();
};

exports.testIsHiddenDoubleDot = (test) => {
    const filename = "..";
    test.ok(!FileUtil.isHidden(filename));
    test.done();
};

exports.testIsHiddenHiddenFile = (test) => {
    const filename = ".gitignore";
    test.ok(FileUtil.isHidden(filename));
    test.done();
};

exports.testIsHiddenNotHiddenFile = (test) => {
    const filename = "file.txt";
    test.ok(!FileUtil.isHidden(filename));
    test.done();
};

/***************************************************************************
 * expandPath tests
 **************************************************************************/
exports.testExpandPathPathWithTilde = (test) => {
    const filepath = "~/filename.txt";
    const expected = process.env.HOME + "/filename.txt";
    test.ok(FileUtil.expandPath(filepath) === expected);
    test.done();
};

exports.testExpandPathPathNoTilde = (test) => {
    const filepath = "./filename.txt";
    test.ok(FileUtil.expandPath(filepath) === filepath);
    test.done();
};
