//
//  FileUtilTests.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

//import Cocoa
import Foundation
import XCTest

class FileUtilTests: XCTestCase {

    override func setUp() {
        super.setUp()
    }

    override func tearDown() {
        super.tearDown()
    }

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
        let hasTxt = FileUtil.hasExtension("filename.txt", ext: "txt")
        XCTAssert(FileUtil.hasExtension("filename.txt", ext: "txt"),
            "hasExtension(\"filename.txt\", \"txt\") == true")
        XCTAssert(FileUtil.hasExtension("filename.TXT", ext: "txt"),
            "hasExtension(\"filename.TXT\", \"txt\") == true")
    }
}
