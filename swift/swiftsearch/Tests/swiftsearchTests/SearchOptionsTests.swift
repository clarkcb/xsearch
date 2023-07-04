//
//  SearchOptionsTests.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Cocoa
import XCTest

import swiftsearch
import swiftfind

class SearchOptionsTests: XCTestCase {
    let options = SearchOptions()
    let startPath: String = "."
    let searchString: String = "Searcher"
    var requiredArgs: [String] = []

    override func setUp() {
        super.setUp()
        requiredArgs = ["--searchpattern", searchString, startPath]
    }

    override func tearDown() {
        super.tearDown()
    }

    func testSettingsEqualDefaultSettings() {
        let settings: SearchSettings = try! options.settingsFromArgs(requiredArgs)
        XCTAssert(settings.archivesOnly == DefaultFindSettings.archivesOnly, "archivesOnly == false")
        XCTAssert(settings.debug == DefaultFindSettings.debug, "debug == false")
        XCTAssert(settings.excludeHidden == DefaultFindSettings.excludeHidden, "excludeHidden == true")
        XCTAssert(settings.firstMatch == DefaultSearchSettings.firstMatch, "firstMatch == false")
        XCTAssert(settings.listDirs == DefaultFindSettings.listDirs, "listDirs == false")
        XCTAssert(settings.listFiles == DefaultFindSettings.listFiles, "listFiles == false")
        XCTAssert(settings.listLines == DefaultSearchSettings.listLines, "listLines == false")
        XCTAssert(settings.maxSize == DefaultFindSettings.maxSize, "maxSize == 0")
        XCTAssert(settings.minSize == DefaultFindSettings.minSize, "maxSize == 0")
        XCTAssert(settings.multiLineSearch == DefaultSearchSettings.multiLineSearch,
                  "multiLineSearch == false")
        XCTAssert(settings.printResults == DefaultSearchSettings.printResults, "printResults == true")
        XCTAssert(settings.printUsage == DefaultFindSettings.printUsage, "printUsage == false")
        XCTAssert(settings.printVersion == DefaultFindSettings.printVersion, "printVersion == false")
        XCTAssert(settings.searchArchives == DefaultSearchSettings.searchArchives,
                  "searchArchives == false")
        XCTAssert(settings.sortCaseInsensitive == DefaultFindSettings.sortCaseInsensitive, "sortCaseInsensitive == false")
        XCTAssert(settings.sortDescending == DefaultFindSettings.sortDescending, "sortDescending == false")
        XCTAssert(settings.uniqueLines == DefaultSearchSettings.uniqueLines, "uniqueLines == false")
        XCTAssert(settings.verbose == DefaultFindSettings.verbose, "verbose == false")
    }

    func testSettingsFromArgs() {
        let otherArgs: [String] = ["--in-ext", "scala,swift", "--debug"]
        let settings: SearchSettings = try! options.settingsFromArgs(requiredArgs + otherArgs)
        print("settings: \(settings)")
        XCTAssert(settings.debug, "debug == true")
        XCTAssert(settings.verbose, "verbose == true")
        XCTAssertEqual(2, settings.inExtensions.count)
        XCTAssertTrue(settings.inExtensions.contains("scala"))
        XCTAssertTrue(settings.inExtensions.contains("swift"))
        XCTAssertEqual(1, settings.searchPatterns.count)
        XCTAssertEqual(1, settings.paths.count)
        XCTAssertTrue(settings.paths.contains("."))
    }

    func testSettingsFromJson() {
        let jsonString = """
{
  "path": "/Users/cary/src/xsearch/",
  "in-ext": ["js", "ts"],
  "out-dirpattern": ["_", "ansible", "bak", "build", "chef", "node_module", "target", "test", "typings"],
  "out-filepattern": ["gulpfile", ".min."],
  "searchpattern": "Searcher",
  "linesbefore": 2,
  "linesafter": 2,
  "debug": true,
  "allmatches": false,
  "includehidden": false,
  "listdirs": true,
  "listfiles": true
}
"""
        let settings = try! options.settingsFromJson(jsonString)
        print("settings: \(settings)")
        XCTAssertTrue(settings.debug, "debug == true")
        XCTAssertTrue(settings.excludeHidden, "excludeHidden == true")
        XCTAssertTrue(settings.firstMatch, "firstMatch == true")
        XCTAssertEqual(2, settings.inExtensions.count)
        XCTAssertTrue(settings.inExtensions.contains("js"))
        XCTAssertTrue(settings.inExtensions.contains("ts"))
        XCTAssertEqual(2, settings.linesBefore)
        XCTAssertEqual(2, settings.linesAfter)
        XCTAssertTrue(settings.listDirs)
        XCTAssertTrue(settings.listFiles)
        XCTAssertEqual(9, settings.outDirPatterns.count)
        XCTAssertEqual(1, settings.searchPatterns.count)

        XCTAssertEqual(1, settings.paths.count)
        XCTAssertTrue(settings.paths.contains("/Users/cary/src/xsearch/"))
        XCTAssertTrue(settings.verbose, "verbose == true")
    }
}
