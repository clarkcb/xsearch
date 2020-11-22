//
//  swiftsearchTests.swift
//  swiftsearchTests
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Cocoa
import XCTest

import swiftsearch

class SearchFileTests: XCTestCase {

    func testSearchFileAbsPath() {
        let path = "/Users/cary/src/xsearch/swift/swiftsearch/Sources/swiftsearch/SearchFile.swift"
        let searchFile = SearchFile(filePath: path, fileType: FileType.code)
        XCTAssertEqual(path, searchFile.description())
    }

    func testSearchFileTildePath() {
        let path = "~/src/xsearch/swift/swiftsearch/Sources/swiftsearch/SearchFile.swift"
        let searchFile = SearchFile(filePath: path, fileType: FileType.code)
        XCTAssertEqual(path, searchFile.description())
    }

    func testSearchFileRelPath1() {
        let path = "./SearchFile.swift"
        let searchFile = SearchFile(filePath: path, fileType: FileType.code)
        XCTAssertEqual(path, searchFile.description())
    }

    func testSearchFileRelPath2() {
        let path = "../SearchFile.swift"
        let searchFile = SearchFile(filePath: path, fileType: FileType.code)
        XCTAssertEqual(path, searchFile.description())
    }
}
