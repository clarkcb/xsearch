/// <reference path="../typings/node/node.d.ts"/>
/// <reference path="../src/fileutil.ts"/>
/*
 * fileutil_test.js
 *
 * Some nodeunit tests of fileutil.js
 */

var FileUtil = require('../build/fileutil.js').FileUtil;

/***************************************************************************
 * getExtension tests
 **************************************************************************/
exports.testGetTxtExtension = function(test) {
    var file: string = "filename.txt";
    test.ok(FileUtil.getExtension(file) === "txt", "ext == \"txt\"");
    test.done();
}

exports.testGetMissingExtension = function(test) {
    var file: string = "filename.";
    test.ok(FileUtil.getExtension(file) === "", "ext == \"\"");
    test.done();
}

exports.testGetNoExtension = function(test) {
    var file: string = "filename";
    test.ok(FileUtil.getExtension(file) === "", "ext == \"\"");
    test.done();
}

exports.testGetHiddenTxtExtension = function(test) {
    var file: string = ".filename.txt";
    test.ok(FileUtil.getExtension(file) === "txt", "ext == \"txt\"");
    test.done();
}

exports.testGetHiddenMissingExtension = function(test) {
    var file: string = "filename.";
    test.ok(FileUtil.getExtension(file) === "", "ext == \"\"");
    test.done();
}

exports.testGetHiddenNoExtension = function(test) {
    var file: string = "filename";
    test.ok(FileUtil.getExtension(file) === "", "ext == \"\"");
    test.done();
}

/***************************************************************************
 * getRelativePath tests
 **************************************************************************/
exports.testGetRelativePath = function(test) {
    var filepath: string = "/Users/cary/filename.txt";
    test.ok(FileUtil.getRelativePath(filepath, '.') === "./filename.txt", "relativepath === ./filenametxt");
    test.done();
}

/***************************************************************************
 * isDotDir tests
 **************************************************************************/
exports.testIsDotDirSingleDot = function(test) {
    var filename: string = ".";
    test.ok(FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == true");
    test.done();
}

exports.testIsDotDirDoubleDot = function(test) {
    var filename: string = "..";
    test.ok(FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == true");
    test.done();
}

exports.testIsDotDirNotDotDir = function(test) {
    var filename: string = "~/path";
    test.ok(!FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == false");
    test.done();
}

exports.testIsDotDirPathWithDot = function(test) {
    var filename: string = "./path";
    test.ok(!FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == false");
    test.done();
}

exports.testIsDotDirHiddenFile = function(test) {
    var filename: string = ".gitignore";
    test.ok(!FileUtil.isDotDir(filename), "isDotDir(" + filename + ") == false");
    test.done();
}

/***************************************************************************
 * isHidden tests
 **************************************************************************/
exports.testIsHiddenSingleDot = function(test) {
    var filename: string = ".";
    test.ok(!FileUtil.isHidden(filename));
    test.done();
}

exports.testIsHiddenDoubleDot = function(test) {
    var filename: string = "..";
    test.ok(!FileUtil.isHidden(filename));
    test.done();
}

exports.testIsHiddenHiddenFile = function(test) {
    var filename: string = ".gitignore";
    test.ok(FileUtil.isHidden(filename));
    test.done();
}

exports.testIsHiddenNotHiddenFile = function(test) {
    var filename: string = "file.txt";
    test.ok(!FileUtil.isHidden(filename));
    test.done();
}

/***************************************************************************
 * expandPath tests
 **************************************************************************/
exports.testExpandPathPathWithTilde = function(test) {
    var filepath: string = "~/filename.txt";
    var expected: string = process.env.HOME + "/filename.txt";
    test.ok(FileUtil.expandPath(filepath) === expected);
    test.done();
}

exports.testExpandPathPathNoTilde = function(test) {
    var filepath: string = "./filename.txt";
    test.ok(FileUtil.expandPath(filepath) === filepath);
    test.done();
}
