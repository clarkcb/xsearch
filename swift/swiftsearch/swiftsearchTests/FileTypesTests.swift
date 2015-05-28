//
//  swiftsearchTests.swift
//  swiftsearchTests
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Cocoa
import XCTest

class FileTypesTests: XCTestCase {

    let fileTypes = FileTypes()

    override func setUp() {
        super.setUp()
    }

    override func tearDown() {
        super.tearDown()
    }

    func testArchiveFiles() {
        let exts = ["7z", "arj", "bz2", "cpio", "ear", "gz", "hqx", "jar",
            "pax", "rar", "sit", "sitx", "tar", "tgz", "war", "zip", "zipx",
            "Z"]
        for x in exts {
            let fileName = "archive.\(x)"
            XCTAssert(fileTypes.isArchiveFile(fileName), "isArchiveFile(\(fileName)) == true")
            XCTAssert(!fileTypes.isBinaryFile(fileName), "isBinaryFile(\(fileName)) == false")
            XCTAssert(!fileTypes.isTextFile(fileName), "isTextFile(\(fileName)) == false")
            XCTAssert(fileTypes.getFileType(fileName) == FileType.Archive,
                "\(fileName) is archive file")
        }
    }

    func testBinaryFiles() {
        let exts = ["a", "ai", "beam", "bin", "chm", "class", "com", "dat",
            "dbmdl", "dcr", "dir", "dll", "dms", "doc", "dot", "dxr", "dylib",
            "epub", "exe", "fm", "hi", "hlp", "indd", "lib", "lnk", "mdb", "mo",
            "mobi", "mpp", "nib", "o", "obj", "odm", "odt", "ott", "pages",
            "pdb", "ppt", "psd", "pub", "pyc", "pyo", "qxd", "rpt", "so", "swf",
            "sys", "vsd", "wpd", "wps", "wpt", "wri", "xls", "xlt"]
        for x in exts {
            let fileName = "binfile.\(x)"
            XCTAssert(!fileTypes.isArchiveFile(fileName), "isArchiveFile(\(fileName)) == false")
            XCTAssert(fileTypes.isBinaryFile(fileName), "isBinaryFile(\(fileName)) == true")
            XCTAssert(!fileTypes.isTextFile(fileName), "isTextFile(\(fileName)) == false")
            XCTAssert(fileTypes.getFileType(fileName) == FileType.Binary,
                "\(fileName) is binary file")
        }
    }

    func testGetFileTypeTextFile() {
        let fileName = "code.swift"
        XCTAssert(fileTypes.isTextFile(fileName), "isTextFile(\(fileName)) == true")
        XCTAssert(fileTypes.getFileType(fileName) == FileType.Text, "\(fileName) is binary file")
    }

    func testGetFileTypeUnknownFile() {
        let fileName = "unknown.ZZZ"
        XCTAssert(fileTypes.getFileType(fileName) == FileType.Unknown, "\(fileName) is unknown file")
    }
}
