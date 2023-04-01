import Cocoa
import XCTest

import swiftsearch

class SearcherTests: XCTestCase {
    let config = Config()
    let fileTypes = FileTypes()

    func getSettings() -> SearchSettings {
        let settings = SearchSettings()
        settings.addPath(".")
        settings.addSearchPattern("Searcher")
        return settings
    }

    override func setUp() {
        super.setUp()
    }

    override func tearDown() {
        super.tearDown()
    }

    /* ==========================================================================
     * isSearchDir tests
     ========================================================================= */
    func testIsSearchDir_SingleDot_True() {
        let settings = getSettings()
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isSearchDir("."))
    }

    func testIsSearchDir_DoubleDot_True() {
        let settings = getSettings()
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isSearchDir(".."))
    }

    func testIsSearchDir_IsHidden_False() {
        let settings = getSettings()
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.isSearchDir(".git"))
    }

    func testIsSearchDir_IsHiddenIncludeHidden_True() {
        let settings = getSettings()
        settings.excludeHidden = false
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isSearchDir(".git"))
    }

    func testIsSearchDir_NoPatterns_True() {
        let settings = getSettings()
        settings.excludeHidden = false
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isSearchDir("/Users"))
    }

    func testIsSearchDir_MatchesInPattern_True() {
        let settings = getSettings()
        settings.addInDirPattern("Search")
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isSearchDir("CsSearch"))
    }

    func testIsSearchDir_DoesNotMatchInPattern_False() {
        let settings = getSettings()
        settings.addInDirPattern("SearchFiles")
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.isSearchDir("CsSearch"))
    }

    func testIsSearchDir_MatchesOutPattern_False() {
        let settings = getSettings()
        settings.addOutDirPattern("Search")
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.isSearchDir("CsSearch"))
    }

    func testIsSearchDir_DoesNotMatchOutPattern_True() {
        let settings = getSettings()
        settings.addOutDirPattern("SearchFiles")
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isSearchDir("CsSearch"))
    }

    /* ==========================================================================
     * isSearchFile tests
     ========================================================================= */
    func testIsSearchFile_NoExtensionsNoPatterns_True() {
        let settings = getSettings()
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isSearchFile("FileUtil.cs"))
    }

    func testIsSearchFile_MatchesInExtension_True() {
        let settings = getSettings()
        settings.addInExtension("cs")
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isSearchFile("FileUtil.cs"))
    }

    func testIsSearchFile_DoesNotMatchInExtension_False() {
        let settings = getSettings()
        settings.addInExtension("java")
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.isSearchFile("FileUtil.cs"))
    }

    func testIsSearchFile_MatchesOutExtension_False() {
        let settings = getSettings()
        settings.addOutExtension("cs")
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.isSearchFile("FileUtil.cs"))
    }

    func testIsSearchFile_DoesNotMatchOutExtension_True() {
        let settings = getSettings()
        settings.addOutExtension("java")
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isSearchFile("FileUtil.cs"))
    }

    func testIsSearchFile_MatchesInFilePattern_True() {
        let settings = getSettings()
        settings.addInFilePattern("Search")
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isSearchFile("Searcher.cs"))
    }

    func testIsSearchFile_DoesNotMatchInFilePattern_False() {
        let settings = getSettings()
        settings.addInFilePattern("Search")
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.isSearchFile("FileUtil.cs"))
    }

    func testIsSearchFile_MatchesOutFilePattern_False() {
        let settings = getSettings()
        settings.addOutFilePattern("Search")
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.isSearchFile("Searcher.cs"))
    }

    func testIsSearchFile_DoesNotMatchOutFilePattern_True() {
        let settings = getSettings()
        settings.addOutFilePattern("Search")
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isSearchFile("FileUtil.cs"))
    }

    /* ==========================================================================
     * isArchiveSearchFile tests
     ========================================================================= */
    func testIsArchiveSearchFile_NoExtensionsNoPatterns_True() {
        let settings = getSettings()
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_MatchesInExtension_True() {
        let settings = getSettings()
        settings.addInArchiveExtension("zip")
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_DoesNotMatchInExtension_False() {
        let settings = getSettings()
        settings.addInArchiveExtension("gz")
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_MatchesOutExtension_False() {
        let settings = getSettings()
        settings.addOutArchiveExtension("zip")
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_DoesNotMatchOutExtension_True() {
        let settings = getSettings()
        settings.addOutArchiveExtension("gz")
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_MatchesInArchiveFilePattern_True() {
        let settings = getSettings()
        settings.addInArchiveFilePattern("arch")
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_DoesNotMatchInArchiveFilePattern_False() {
        let settings = getSettings()
        settings.addInArchiveFilePattern("archives")
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_MatchesOutArchiveFilePattern_False() {
        let settings = getSettings()
        settings.addOutArchiveFilePattern("arch")
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_DoesNotMatchOutArchiveFilePattern_True() {
        let settings = getSettings()
        settings.addOutArchiveFilePattern("archives")
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.isArchiveSearchFile("archive.zip"))
    }

    /* ==========================================================================
     * filterFile tests
     ========================================================================= */
    func testFilterFile_IsHidden_False() {
        let settings = getSettings()
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.filterFile(".gitignore"))
    }

    func testFilterFile_IsHiddenIncludeHidden_True() {
        let settings = getSettings()
        settings.excludeHidden = false
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.filterFile(".hidden.txt"))
    }

    func testFilterFile_ArchiveNoSearchArchives_False() {
        let settings = getSettings()
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.filterFile("archive.zip"))
    }

    func testFilterFile_ArchiveSearchArchives_True() {
        let settings = getSettings()
        settings.searchArchives = true
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.filterFile("archive.zip"))
    }

    func testFilterFile_IsArchiveSearchFile_True() {
        let settings = getSettings()
        settings.searchArchives = true
        settings.addInArchiveExtension("zip")
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.filterFile("archive.zip"))
    }

    func testFilterFile_NotIsArchiveSearchFile_False() {
        let settings = getSettings()
        settings.searchArchives = true
        settings.addOutArchiveExtension("zip")
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.filterFile("archive.zip"))
    }

    func testFilterFile_ArchiveFileArchivesOnly_True() {
        let settings = getSettings()
        settings.archivesOnly = true
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.filterFile("archive.zip"))
    }

    func testFilterFile_NoExtensionsNoPatterns_True() {
        let settings = getSettings()
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.filterFile("FileUtil.cs"))
    }

    func testFilterFile_IsSearchFile_True() {
        let settings = getSettings()
        settings.addInExtension("cs")
        let searcher = try! Searcher(settings: settings)
        XCTAssertTrue(searcher.filterFile("FileUtil.cs"))
    }

    func testFilterFile_NotIsSearchFile_False() {
        let settings = getSettings()
        settings.addOutExtension("cs")
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.filterFile("FileUtil.cs"))
    }

    func testFilterFile_NonArchiveFileArchivesOnly_False() {
        let settings = getSettings()
        settings.archivesOnly = true
        let searcher = try! Searcher(settings: settings)
        XCTAssertFalse(searcher.filterFile("FileUtil.cs"))
    }

    /* ==========================================================================
     * searchLineReader tests
     ========================================================================= */
    func testSearchLineReader() {
        let settings = getSettings()
        let searcher = try! Searcher(settings: settings)
        let testFilePath = FileUtil.joinPath(config.sharedPath, childPath: "testFiles/testFile2.txt")

        if let reader = StreamReader(path: testFilePath, encoding: .utf8) {
            let results = searcher.searchLineReader(reader)

            XCTAssert(results.count == 2)

            XCTAssertEqual(29, results[0].lineNum)
            XCTAssertEqual(3, results[0].matchStartIndex)
            XCTAssertEqual(11, results[0].matchEndIndex)

            XCTAssertEqual(35, results[1].lineNum)
            XCTAssertEqual(24, results[1].matchStartIndex)
            XCTAssertEqual(32, results[1].matchEndIndex)

            reader.close()

        } else {
            XCTAssertTrue(false)
        }
    }

    /* ==========================================================================
     * searchMultiLineString tests
     ========================================================================= */
    func testSearchMultiLineString() {
        let settings = getSettings()
        settings.multiLineSearch = true
        let searcher = try! Searcher(settings: settings)
        let testFilePath = FileUtil.joinPath(config.sharedPath, childPath: "testFiles/testFile2.txt")
        let testFileContents = try? String(contentsOfFile: testFilePath, encoding: .utf8)
        if testFileContents != nil {
            let results = searcher.searchMultiLineString(testFileContents!)

            XCTAssert(results.count == 2)

            XCTAssertEqual(29, results[0].lineNum)
            XCTAssertEqual(3, results[0].matchStartIndex)
            XCTAssertEqual(11, results[0].matchEndIndex)

            XCTAssertEqual(35, results[1].lineNum)
            XCTAssertEqual(24, results[1].matchStartIndex)
            XCTAssertEqual(32, results[1].matchEndIndex)

        } else {
            XCTAssertTrue(false)
        }
    }
}
