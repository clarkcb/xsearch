import Cocoa
import XCTest

import swiftsearch
import swiftfind

class SearcherTests: XCTestCase {
    let config = SearchConfig()
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
