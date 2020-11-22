//
//  SearchSettingsTests.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Cocoa
import XCTest

import swiftsearch

class SearchSettingsTests: XCTestCase {
    override func setUp() {
        super.setUp()
    }

    override func tearDown() {
        super.tearDown()
    }

    func testDefaultSettings() {
        XCTAssert(DefaultSettings.archivesOnly == false, "archivesOnly == false")
        XCTAssert(DefaultSettings.colorize == true, "colorize == true")
        XCTAssert(DefaultSettings.debug == false, "debug == false")
        XCTAssert(DefaultSettings.excludeHidden == true, "excludeHidden == true")
        XCTAssert(DefaultSettings.firstMatch == false, "firstMatch == false")
        XCTAssert(DefaultSettings.listDirs == false, "listDirs == false")
        XCTAssert(DefaultSettings.listFiles == false, "listFiles == false")
        XCTAssert(DefaultSettings.listLines == false, "listLines == false")
        XCTAssert(DefaultSettings.multiLineSearch == false, "multiLineSearch == false")
        XCTAssert(DefaultSettings.printResults == true, "printResults == true")
        XCTAssert(DefaultSettings.printUsage == false, "printUsage == false")
        XCTAssert(DefaultSettings.printVersion == false, "printVersion == false")
        XCTAssert(DefaultSettings.searchArchives == false, "searchArchives == false")
        XCTAssert(DefaultSettings.uniqueLines == false, "uniqueLines == false")
        XCTAssert(DefaultSettings.verbose == false, "verbose == false")
    }

    func testInitialSettingsEqualDefaultSettings() {
        let settings = SearchSettings()
        XCTAssert(settings.archivesOnly == DefaultSettings.archivesOnly, "archivesOnly == false")
        XCTAssert(settings.colorize == DefaultSettings.colorize, "colorize == true")
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

    func testAddExtensions() {
        let settings = SearchSettings()
        settings.addInExtension("java")
        settings.addInExtension("scala")
        settings.addInExtension("cs,fs")
        XCTAssert(settings.inExtensions.count == 4)
        XCTAssert(settings.inExtensions.contains("java"))
        XCTAssert(settings.inExtensions.contains("scala"))
        XCTAssert(settings.inExtensions.contains("cs"))
        XCTAssert(settings.inExtensions.contains("fs"))
    }

    func testAddPattern() {
        let settings = SearchSettings()
        settings.addSearchPattern("Searcher")
    }

    func testSetArchivesOnly() {
        let settings = SearchSettings()
        XCTAssertFalse(settings.archivesOnly)
        XCTAssertFalse(settings.searchArchives)
        settings.archivesOnly = true
        XCTAssertTrue(settings.archivesOnly)
        XCTAssertTrue(settings.searchArchives)
    }

    func testSetDebug() {
        let settings = SearchSettings()
        XCTAssertFalse(settings.debug)
        XCTAssertFalse(settings.verbose)
        settings.debug = true
        XCTAssertTrue(settings.debug)
        XCTAssertTrue(settings.verbose)
    }
}
