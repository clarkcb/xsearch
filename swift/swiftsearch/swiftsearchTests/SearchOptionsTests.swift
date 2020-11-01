//
//  SearchOptionsTests.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Cocoa
import XCTest

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
        var error: NSError?
        let settings: SearchSettings = options.settingsFromArgs(requiredArgs, error: &error)
        XCTAssert(settings.archivesOnly == DefaultSettings.archivesOnly, "archivesOnly == false")
        XCTAssert(settings.debug == DefaultSettings.debug, "debug == false")
        XCTAssert(settings.excludeHidden == DefaultSettings.excludeHidden, "excludeHidden == true")
        XCTAssert(settings.firstMatch == DefaultSettings.firstMatch, "firstMatch == false")
        XCTAssert(settings.listDirs == DefaultSettings.listDirs, "listDirs == false")
        XCTAssert(settings.listFiles == DefaultSettings.listFiles, "listFiles == false")
        XCTAssert(settings.listLines == DefaultSettings.listLines, "listLines == false")
        XCTAssert(settings.multiLineSearch == DefaultSettings.multiLineSearch,
                  "multiLineSearch == false")
        XCTAssert(settings.printResults == DefaultSettings.printResults, "printResults == true")
        XCTAssert(settings.printUsage == DefaultSettings.printUsage, "printUsage == false")
        XCTAssert(settings.printVersion == DefaultSettings.printVersion, "printVersion == false")
        XCTAssert(settings.searchArchives == DefaultSettings.searchArchives,
                  "searchArchives == false")
        XCTAssert(settings.uniqueLines == DefaultSettings.uniqueLines, "uniqueLines == false")
        XCTAssert(settings.verbose == DefaultSettings.verbose, "verbose == false")
    }

    func testSettingsFromArgs() {
        var error: NSError?
        let otherArgs: [String] = ["--in-ext", "scala,swift", "--debug"]
        let settings: SearchSettings = options.settingsFromArgs(requiredArgs + otherArgs, error: &error)
        print("settings: \(settings)")
        XCTAssert(settings.debug, "debug == true")
        XCTAssert(settings.verbose, "verbose == true")
    }
}
