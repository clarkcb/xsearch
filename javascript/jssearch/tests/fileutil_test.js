/*
 * fileutil_test.js
 *
 * Some nodeunit tests of fileutil.js
 */

var FileUtil = require('../src/fileutil.js').FileUtil;

/***************************************************************************
 * getExtension tests
 **************************************************************************/
exports.testGetTxtExtension = function(test) {
    const file = "filename.txt";
    test.ok(FileUtil.getExtension(file) === "txt", "ext == \"txt\"");
    test.done();
};

exports.testGetMissingExtension = function(test) {
    const file = "filename.";
    test.ok(FileUtil.getExtension(file) === "", "ext == \"\"");
    test.done();
};

exports.testGetNoExtension = function(test) {
    const file = "filename";
    test.ok(FileUtil.getExtension(file) === "", "ext == \"\"");
    test.done();
};

exports.testGetHiddenTxtExtension = function(test) {
    const file = ".filename.txt";
    test.ok(FileUtil.getExtension(file) === "txt", "ext == \"txt\"");
    test.done();
};

exports.testGetHiddenMissingExtension = function(test) {
    const file = "filename.";
    test.ok(FileUtil.getExtension(file) === "", "ext == \"\"");
    test.done();
};

exports.testGetHiddenNoExtension = function(test) {
    const file = "filename";
    test.ok(FileUtil.getExtension(file) === "", "ext == \"\"");
    test.done();
};

/***************************************************************************
 * getRelativePath tests
 **************************************************************************/
exports.testGetRelativePath = function(test) {
    const filepath = "/Users/cary/filename.txt";
    test.ok(FileUtil.getRelativePath(filepath, '.') === "./filename.txt", "relativepath === ./filenametxt");
    test.done();
};

/***************************************************************************
 * isDotDir tests
 **************************************************************************/
exports.testIsDotDirSingleDot = function(test) {
    const filename = ".";
    test.ok(FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == true");
    test.done();
};

exports.testIsDotDirDoubleDot = function(test) {
    const filename = "..";
    test.ok(FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == true");
    test.done();
};

exports.testIsDotDirNotDotDir = function(test) {
    const filename = "~/path";
    test.ok(!FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == false");
    test.done();
};

exports.testIsDotDirPathWithDot = function(test) {
    const filename = "./path";
    test.ok(!FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == false");
    test.done();
};

exports.testIsDotDirHiddenFile = function(test) {
    const filename = ".gitignore";
    test.ok(!FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == false");
    test.done();
};

/***************************************************************************
 * isHidden tests
 **************************************************************************/
exports.testIsHiddenSingleDot = function(test) {
    const filename = ".";
    test.ok(!FileUtil.isHidden(filename));
    test.done();
};

exports.testIsHiddenDoubleDot = function(test) {
    const filename = "..";
    test.ok(!FileUtil.isHidden(filename));
    test.done();
};

exports.testIsHiddenHiddenFile = function(test) {
    const filename = ".gitignore";
    test.ok(FileUtil.isHidden(filename));
    test.done();
};

exports.testIsHiddenNotHiddenFile = function(test) {
    const filename = "file.txt";
    test.ok(!FileUtil.isHidden(filename));
    test.done();
};

/***************************************************************************
 * expandPath tests
 **************************************************************************/
exports.testExpandPathPathWithTilde = function(test) {
    const filepath = "~/filename.txt";
    const expected = process.env.HOME + "/filename.txt";
    test.ok(FileUtil.expandPath(filepath) === expected);
    test.done();
};

exports.testExpandPathPathNoTilde = function(test) {
    const filepath = "./filename.txt";
    test.ok(FileUtil.expandPath(filepath) === filepath);
    test.done();
};
