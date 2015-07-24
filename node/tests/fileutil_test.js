/*
 * fileutil_test.js
 *
 * Some nodeunit tests of fileutil.js
 */

var FileUtil = require('../nodesearch/fileutil.js').FileUtil;

/***************************************************************************
 * getExtension tests
 **************************************************************************/
exports.testGetTxtExtension = function(test) {
    var file = "filename.txt";
    test.ok(FileUtil.getExtension(file) === "txt", "ext == \"txt\"");
    test.done();
}

exports.testGetMissingExtension = function(test) {
    var file = "filename.";
    test.ok(FileUtil.getExtension(file) === "", "ext == \"\"");
    test.done();
}

exports.testGetNoExtension = function(test) {
    var file = "filename";
    test.ok(FileUtil.getExtension(file) === "", "ext == \"\"");
    test.done();
}

exports.testGetHiddenTxtExtension = function(test) {
    var file = ".filename.txt";
    test.ok(FileUtil.getExtension(file) === "txt", "ext == \"txt\"");
    test.done();
}

exports.testGetHiddenMissingExtension = function(test) {
    var file = "filename.";
    test.ok(FileUtil.getExtension(file) === "", "ext == \"\"");
    test.done();
}

exports.testGetHiddenNoExtension = function(test) {
    var file = "filename";
    test.ok(FileUtil.getExtension(file) === "", "ext == \"\"");
    test.done();
}

/***************************************************************************
 * getRelativePath tests
 **************************************************************************/
exports.testGetRelativePath = function(test) {
    var filepath = "/Users/cary/filename.txt";
    test.ok(FileUtil.getRelativePath(filepath, '.') === "./filename.txt", "relativepath === ./filenametxt");
    test.done();
}

/***************************************************************************
 * isDotDir tests
 **************************************************************************/
exports.testIsDotDirSingleDot = function(test) {
    var filename = ".";
    test.ok(FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == true");
    test.done();
}

exports.testIsDotDirDoubleDot = function(test) {
    var filename = "..";
    test.ok(FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == true");
    test.done();
}

exports.testIsDotDirNotDotDir = function(test) {
    var filename = "~/path";
    test.ok(!FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == false");
    test.done();
}

exports.testIsDotDirPathWithDot = function(test) {
    var filename = "./path";
    test.ok(!FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == false");
    test.done();
}

exports.testIsDotDirHiddenFile = function(test) {
    var filename = ".gitignore";
    test.ok(!FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == false");
    test.done();
}

/***************************************************************************
 * isHidden tests
 **************************************************************************/
exports.testIsHiddenSingleDot = function(test) {
    var filename = ".";
    test.ok(!FileUtil.isHidden(filename));
    test.done();
}

exports.testIsHiddenDoubleDot = function(test) {
    var filename = "..";
    test.ok(!FileUtil.isHidden(filename));
    test.done();
}

exports.testIsHiddenHiddenFile = function(test) {
    var filename = ".gitignore";
    test.ok(FileUtil.isHidden(filename));
    test.done();
}

exports.testIsHiddenNotHiddenFile = function(test) {
    var filename = "file.txt";
    test.ok(!FileUtil.isHidden(filename));
    test.done();
}

/***************************************************************************
 * expandPath tests
 **************************************************************************/
exports.testExpandPathPathWithTilde = function(test) {
    var filepath = "~/filename.txt";
    var expected = process.env.HOME + "/filename.txt";
    test.ok(FileUtil.expandPath(filepath) === expected);
    test.done();
}

exports.testExpandPathPathNoTilde = function(test) {
    var filepath = "./filename.txt";
    test.ok(FileUtil.expandPath(filepath) === filepath);
    test.done();
}
