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
import swiftfind

class SearchSettingsTests: XCTestCase {
    override func setUp() {
        super.setUp()
    }

    override func tearDown() {
        super.tearDown()
    }

    func testDefaultSettings() {
        XCTAssert(DefaultFindSettings.archivesOnly == false, "archivesOnly == false")
        XCTAssert(DefaultSearchSettings.colorize == true, "colorize == true")
        XCTAssert(DefaultFindSettings.debug == false, "debug == false")
        XCTAssert(DefaultSearchSettings.firstMatch == false, "firstMatch == false")
        XCTAssert(DefaultFindSettings.includeHidden == false, "includeHidden == false")
        XCTAssert(DefaultSearchSettings.maxLineLength == 150, "maxLineLength == 150")
        XCTAssert(DefaultFindSettings.maxLastMod == nil, "maxLastMod == nil")
        XCTAssert(DefaultFindSettings.maxSize == 0, "maxSize == 0")
        XCTAssert(DefaultFindSettings.minLastMod == nil, "minLastMod == nil")
        XCTAssert(DefaultFindSettings.minSize == 0, "minSize == 0")
        XCTAssert(DefaultSearchSettings.multiLineSearch == false, "multiLineSearch == false")
        XCTAssert(DefaultFindSettings.printDirs == false, "printDirs == false")
        XCTAssert(DefaultFindSettings.printFiles == false, "printFiles == false")
        XCTAssert(DefaultSearchSettings.printLines == false, "printLines == false")
        XCTAssert(DefaultSearchSettings.printResults == true, "printResults == true")
        XCTAssert(DefaultFindSettings.printUsage == false, "printUsage == false")
        XCTAssert(DefaultFindSettings.printVersion == false, "printVersion == false")
        XCTAssert(DefaultSearchSettings.searchArchives == false, "searchArchives == false")
        XCTAssert(DefaultFindSettings.sortCaseInsensitive == false, "sortCaseInsensitive == false")
        XCTAssert(DefaultFindSettings.sortDescending == false, "sortDescending == false")
        XCTAssert(DefaultSearchSettings.uniqueLines == false, "uniqueLines == false")
        XCTAssert(DefaultFindSettings.verbose == false, "verbose == false")
    }

    func testInitialSettingsEqualDefaultSettings() {
        let settings = SearchSettings()
        XCTAssert(settings.archivesOnly == DefaultFindSettings.archivesOnly, "archivesOnly == false")
        XCTAssert(settings.colorize == DefaultSearchSettings.colorize, "colorize == true")
        XCTAssert(settings.debug == DefaultFindSettings.debug, "debug == false")
        XCTAssert(settings.firstMatch == DefaultSearchSettings.firstMatch, "firstMatch == false")
        XCTAssert(settings.includeHidden == DefaultFindSettings.includeHidden, "includeHidden == false")
        XCTAssert(settings.maxLineLength == DefaultSearchSettings.maxLineLength, "maxLineLength == 150")
        XCTAssert(settings.maxLastMod == DefaultFindSettings.maxLastMod, "maxLastMod == nil")
        XCTAssert(settings.maxSize == DefaultFindSettings.maxSize, "maxSize == 0")
        XCTAssert(settings.minLastMod == DefaultFindSettings.minLastMod, "minLastMod == nil")
        XCTAssert(settings.minSize == DefaultFindSettings.minSize, "maxSize == 0")
        XCTAssert(settings.multiLineSearch == DefaultSearchSettings.multiLineSearch,
                  "multiLineSearch == false")
        XCTAssert(settings.printDirs == DefaultFindSettings.listDirs, "printDirs == false")
        XCTAssert(settings.printFiles == DefaultFindSettings.listFiles, "printFiles == false")
        XCTAssert(settings.printLines == DefaultSearchSettings.printLines, "printLines == false")
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
