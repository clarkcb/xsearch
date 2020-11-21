import Cocoa
import XCTest

import swiftsearch

class SearcherTests: XCTestCase {
    let fileTypes = FileTypes()

    func getSettings() -> SearchSettings {
        let settings = SearchSettings()
        settings.startPath = "."
        settings.searchPatterns.append(Regex("Searcher"))
        return settings
    }

    override func setUp() {
        super.setUp()
    }

    override func tearDown() {
        super.tearDown()
    }

    func testOneOffTest() {
        var error: NSError?
        let settings = getSettings()
        let searcher = Searcher(settings: settings, error: &error)
        settings.startPath = "/Users/cary/src/xsearch/python/pysearch/venv/lib/python3.6/site-packages/pip/_vendor/appdirs.py"

        searcher.search(&error)
    }
}
