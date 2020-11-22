import Cocoa
import XCTest

import swiftsearch

class SearcherTests: XCTestCase {
    let fileTypes = FileTypes()

    func getSettings() -> SearchSettings {
        let settings = SearchSettings()
        settings.startPath = "."
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
        var error: NSError?
        let settings = getSettings()
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isSearchDir("."))
    }

    func testIsSearchDir_DoubleDot_True() {
        var error: NSError?
        let settings = getSettings()
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isSearchDir(".."))
    }

    func testIsSearchDir_IsHidden_False() {
        var error: NSError?
        let settings = getSettings()
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.isSearchDir(".git"))
    }

    func testIsSearchDir_IsHiddenIncludeHidden_True() {
        var error: NSError?
        let settings = getSettings()
        settings.excludeHidden = false
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isSearchDir(".git"))
    }

    func testIsSearchDir_NoPatterns_True() {
        var error: NSError?
        let settings = getSettings()
        settings.excludeHidden = false
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isSearchDir("/Users"))
    }

    func testIsSearchDir_MatchesInPattern_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addInDirPattern("Search")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isSearchDir("CsSearch"))
    }

    func testIsSearchDir_DoesNotMatchInPattern_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addInDirPattern("SearchFiles")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.isSearchDir("CsSearch"))
    }

    func testIsSearchDir_MatchesOutPattern_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutDirPattern("Search")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.isSearchDir("CsSearch"))
    }

    func testIsSearchDir_DoesNotMatchOutPattern_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutDirPattern("SearchFiles")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isSearchDir("CsSearch"))
    }

    /* ==========================================================================
     * isSearchFile tests
     ========================================================================= */
    func testIsSearchFile_NoExtensionsNoPatterns_True() {
        var error: NSError?
        let settings = getSettings()
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isSearchFile("FileUtil.cs"))
    }

    func testIsSearchFile_MatchesInExtension_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addInExtension("cs")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isSearchFile("FileUtil.cs"))
    }

    func testIsSearchFile_DoesNotMatchInExtension_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addInExtension("java")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.isSearchFile("FileUtil.cs"))
    }

    func testIsSearchFile_MatchesOutExtension_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutExtension("cs")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.isSearchFile("FileUtil.cs"))
    }

    func testIsSearchFile_DoesNotMatchOutExtension_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutExtension("java")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isSearchFile("FileUtil.cs"))
    }

    func testIsSearchFile_MatchesInFilePattern_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addInFilePattern("Search")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isSearchFile("Searcher.cs"))
    }

    func testIsSearchFile_DoesNotMatchInFilePattern_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addInFilePattern("Search")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.isSearchFile("FileUtil.cs"))
    }

    func testIsSearchFile_MatchesOutFilePattern_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutFilePattern("Search")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.isSearchFile("Searcher.cs"))
    }

    func testIsSearchFile_DoesNotMatchOutFilePattern_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutFilePattern("Search")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isSearchFile("FileUtil.cs"))
    }

    /* ==========================================================================
     * isArchiveSearchFile tests
     ========================================================================= */
    func testIsArchiveSearchFile_NoExtensionsNoPatterns_True() {
        var error: NSError?
        let settings = getSettings()
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_MatchesInExtension_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addInArchiveExtension("zip")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_DoesNotMatchInExtension_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addInArchiveExtension("gz")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_MatchesOutExtension_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutArchiveExtension("zip")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_DoesNotMatchOutExtension_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutArchiveExtension("gz")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_MatchesInArchiveFilePattern_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addInArchiveFilePattern("arch")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_DoesNotMatchInArchiveFilePattern_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addInArchiveFilePattern("archives")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_MatchesOutArchiveFilePattern_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutArchiveFilePattern("arch")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.isArchiveSearchFile("archive.zip"))
    }

    func testIsArchiveSearchFile_DoesNotMatchOutArchiveFilePattern_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutArchiveFilePattern("archives")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.isArchiveSearchFile("archive.zip"))
    }

    /* ==========================================================================
     * filterFile tests
     ========================================================================= */
    func testFilterFile_IsHidden_False() {
        var error: NSError?
        let settings = getSettings()
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.filterFile(".gitignore"))
    }

    func testFilterFile_IsHiddenIncludeHidden_True() {
        var error: NSError?
        let settings = getSettings()
        settings.excludeHidden = false
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.filterFile(".hidden.txt"))
    }

    func testFilterFile_ArchiveNoSearchArchives_False() {
        var error: NSError?
        let settings = getSettings()
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.filterFile("archive.zip"))
    }

    func testFilterFile_ArchiveSearchArchives_True() {
        var error: NSError?
        let settings = getSettings()
        settings.searchArchives = true
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.filterFile("archive.zip"))
    }

    func testFilterFile_IsArchiveSearchFile_True() {
        var error: NSError?
        let settings = getSettings()
        settings.searchArchives = true
        settings.addInArchiveExtension("zip")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.filterFile("archive.zip"))
    }

    func testFilterFile_NotIsArchiveSearchFile_False() {
        var error: NSError?
        let settings = getSettings()
        settings.searchArchives = true
        settings.addOutArchiveExtension("zip")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.filterFile("archive.zip"))
    }

    func testFilterFile_ArchiveFileArchivesOnly_True() {
        var error: NSError?
        let settings = getSettings()
        settings.archivesOnly = true
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.filterFile("archive.zip"))
    }

    func testFilterFile_NoExtensionsNoPatterns_True() {
        var error: NSError?
        let settings = getSettings()
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.filterFile("FileUtil.cs"))
    }

    func testFilterFile_IsSearchFile_True() {
        var error: NSError?
        let settings = getSettings()
        settings.addInExtension("cs")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertTrue(searcher.filterFile("FileUtil.cs"))
    }

    func testFilterFile_NotIsSearchFile_False() {
        var error: NSError?
        let settings = getSettings()
        settings.addOutExtension("cs")
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.filterFile("FileUtil.cs"))
    }

    func testFilterFile_NonArchiveFileArchivesOnly_False() {
        var error: NSError?
        let settings = getSettings()
        settings.archivesOnly = true
        let searcher = Searcher(settings: settings, error: &error)
        XCTAssertFalse(searcher.filterFile("FileUtil.cs"))
    }

    /* ==========================================================================
     * searchLineReader tests
     ========================================================================= */
    func testSearchLineReader() {
        var error: NSError?
        let settings = getSettings()
        let searcher = Searcher(settings: settings, error: &error)
        let testFilePath = FileUtil.joinPath(Config.sharedPath, childPath: "testFiles/testFile2.txt")

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
        var error: NSError?
        let settings = getSettings()
        settings.multiLineSearch = true
        let searcher = Searcher(settings: settings, error: &error)
        let testFilePath = FileUtil.joinPath(Config.sharedPath, childPath: "testFiles/testFile2.txt")
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
