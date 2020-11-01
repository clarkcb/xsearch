//
//  FileUtilTests.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

// import Cocoa
import Foundation
import XCTest

class FileUtilTests: XCTestCase {
    override func setUp() {
        super.setUp()
    }

    override func tearDown() {
        super.tearDown()
    }

    /* ==========================================================================
     * getExtension tests
     ========================================================================= */
    func testGetExtension() {
        XCTAssert(FileUtil.getExtension("filename.txt") == "txt",
                  "getExtension(\"filename.txt\") == \"txt\"")
        XCTAssert(FileUtil.getExtension("filename.TXT") == "txt",
                  "getExtension(\"filename.TXT\") == \"txt\"")
        XCTAssert(FileUtil.getExtension("filename.") == "", "getExtension(\"filename.\") == \"\"")
        XCTAssert(FileUtil.getExtension("filename") == "", "getExtension(\"filename\") == \"\"")
        XCTAssert(FileUtil.getExtension(".hidden.txt") == "txt",
                  "getExtension(\".hidden.txt\") == \"txt\"")
        XCTAssert(FileUtil.getExtension(".hidden.") == "", "getExtension(\".hidden.\") == \"\"")
        XCTAssert(FileUtil.getExtension(".hidden") == "", "getExtension(\".hidden\") == \"\"")
    }

    func testHasExtension() {
        XCTAssert(FileUtil.hasExtension("filename.txt", ext: "txt"),
                  "hasExtension(\"filename.txt\", \"txt\") == true")
        XCTAssert(FileUtil.hasExtension("filename.TXT", ext: "txt"),
                  "hasExtension(\"filename.TXT\", \"txt\") == true")
    }

    /* ==========================================================================
     * isDotDir tests
     ========================================================================= */
    func testIsDotDirSingleDot() {
        let filename = "."
        XCTAssertTrue(FileUtil.isDotDir(filename))
    }

    func testIsDotDirDoubleDot() {
        let filename = ".."
        XCTAssertTrue(FileUtil.isDotDir(filename))
    }

    func testIsDotDirNotDotDir() {
        let filename = "~/path"
        XCTAssertFalse(FileUtil.isDotDir(filename))
    }

    func testIsDotDirPathWithDot() {
        let filename = "./path"
        XCTAssertFalse(FileUtil.isDotDir(filename))
    }

    func testIsDotDirHiddenFile() {
        let filename = ".gitignore"
        XCTAssertFalse(FileUtil.isDotDir(filename))
    }

    /* ==========================================================================
     * isHidden tests
     ========================================================================= */
    func testIsHiddenSingleDot() {
        let filename = "."
        XCTAssertFalse(FileUtil.isHidden(filename))
    }

    func testIsHiddenDoubleDot() {
        let filename = ".."
        XCTAssertFalse(FileUtil.isHidden(filename))
    }

    func testIsHiddenHiddenFileName() {
        let filename = ".gitignore"
        XCTAssertTrue(FileUtil.isHidden(filename))
    }

    func testIsHiddenNotHiddenFileName() {
        let filename = "./file.txt"
        XCTAssertFalse(FileUtil.isHidden(filename))
    }

    /* ==========================================================================
     * joinPath tests
     ========================================================================= */

    func testJoinPathDir() {
        let joined = FileUtil.joinPath("./path/to", childPath: "somewhere")
        XCTAssertEqual(joined, "./path/to/somewhere")
    }

    func testJoinPathFile() {
        let joined = FileUtil.joinPath("./path/to", childPath: "somefile.txt")
        XCTAssertEqual(joined, "./path/to/somefile.txt")
    }

    /* ==========================================================================
     * splitPath tests
     ========================================================================= */

    func testSplitPathDir() {
        let path = "./path/to/somewhere/"
        let (parent, child) = FileUtil.splitPath(path)
        XCTAssertEqual(parent, "./path/to")
        XCTAssertEqual(child, "somewhere")
    }

    func testSplitPathFile() {
        let path = "./path/to/somefile.txt"
        let (parent, child) = FileUtil.splitPath(path)
        XCTAssertEqual(parent, "./path/to")
        XCTAssertEqual(child, "somefile.txt")
    }
}
