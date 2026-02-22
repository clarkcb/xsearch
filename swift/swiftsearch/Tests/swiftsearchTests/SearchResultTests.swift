//
//  SearchResultTests.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Cocoa
import XCTest

import swiftsearch
import swiftfind

class SearchResultTests: XCTestCase {
    func testSingleLineSearchResult() {
        let pattern = "Search"
        let filepath = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        let fileResult = FileResult(filePath: filepath, fileType: FileType.code, fileSize: 0)
        let lineNum = 10
        let matchStartIndex = 15
        let matchEndIndex = 21
        let line = "\tpublic class Searcher\n"
        let searchResult = SearchResult(
            searchPattern: pattern,
            file: fileResult,
            lineNum: lineNum,
            matchStartIndex: matchStartIndex,
            matchEndIndex: matchEndIndex,
            line: line,
            linesBefore: [],
            linesAfter: []
        )
        let expectedLine = line.trimmingCharacters(in: whitespace as CharacterSet)
        let expectedOutput = "\(filepath): \(lineNum): [\(matchStartIndex):\(matchEndIndex)]: \(expectedLine)"

        let settings = SearchSettings()
        settings.colorize = false
        let formatter = SearchResultFormatter(settings: settings)

        let output = formatter.format(result: searchResult)
        XCTAssertEqual(expectedOutput, output, "single-line searchResult matches expected")
    }

    func testSingleLineSearchResultLongerThanMaxLineLength() {
        let pattern = "maxlen"
        let file = "./maxlen.txt"
        let fileResult = FileResult(filePath: file, fileType: FileType.text, fileSize: 0)
        let lineNum = 1
        let matchStartIndex = 53
        let matchEndIndex = 59
        let line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        let linesBeforeAfter: [String] = []
        let searchResult = SearchResult(
            searchPattern: pattern,
            file: fileResult,
            lineNum: lineNum,
            matchStartIndex: matchStartIndex,
            matchEndIndex: matchEndIndex,
            line: line,
            linesBefore: linesBeforeAfter,
            linesAfter: linesBeforeAfter
        )
        let expectedLine = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."
        let expectedOutput = "\(file): \(lineNum): [\(matchStartIndex):\(matchEndIndex)]: \(expectedLine)"

        let settings = SearchSettings()
        settings.colorize = false
        settings.maxLineLength = 100
        let formatter = SearchResultFormatter(settings: settings)

        let output = formatter.format(result: searchResult)
        XCTAssertEqual(expectedOutput, output, "single-line longer than maxLineLength searchResult matches expected")
    }

    func testSingleLineSearchResultLongerColorize() {
        let pattern = "maxlen"
        let file = "./maxlen.txt"
        let fileResult = FileResult(filePath: file, fileType: FileType.text, fileSize: 0)
        let lineNum = 1
        let matchStartIndex = 53
        let matchEndIndex = 59
        let line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        let linesBeforeAfter: [String] = []
        let searchResult = SearchResult(
            searchPattern: pattern,
            file: fileResult,
            lineNum: lineNum,
            matchStartIndex: matchStartIndex,
            matchEndIndex: matchEndIndex,
            line: line,
            linesBefore: linesBeforeAfter,
            linesAfter: linesBeforeAfter
        )
        let expectedLine = "...89012345678901234567890123456789012345678901" +
            ConsoleColor.GREEN +
            "maxlen" +
            ConsoleColor.RESET +
            "89012345678901234567890123456789012345678901..."
        let expectedOutput = "\(file): \(lineNum): [\(matchStartIndex):\(matchEndIndex)]: \(expectedLine)"

        let settings = SearchSettings()
        settings.colorize = true
        settings.maxLineLength = 100
        let formatter = SearchResultFormatter(settings: settings)

        let output = formatter.format(result: searchResult)
        XCTAssertEqual(expectedOutput, output, "single-line colorized searchResult matches expected")
    }

    func testSearchResultMatchLongerColorize() {
        let pattern = "\\d+maxlen\\d+"
        let file = "./maxlen.txt"
        let fileResult = FileResult(filePath: file, fileType: FileType.text, fileSize: 0)
        let lineNum = 1
        let matchStartIndex = 1
        let matchEndIndex = 110
        let line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        let linesBeforeAfter: [String] = []
        let searchResult = SearchResult(
            searchPattern: pattern,
            file: fileResult,
            lineNum: lineNum,
            matchStartIndex: matchStartIndex,
            matchEndIndex: matchEndIndex,
            line: line,
            linesBefore: linesBeforeAfter,
            linesAfter: linesBeforeAfter
        )
        let expectedLine = ConsoleColor.GREEN +
            "0123456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456" +
            ConsoleColor.RESET +
            "..."
        let expectedOutput = "\(file): \(lineNum): [\(matchStartIndex):\(matchEndIndex)]: \(expectedLine)"

        let settings = SearchSettings()
        settings.colorize = true
        settings.maxLineLength = 100
        let formatter = SearchResultFormatter(settings: settings)

        let output = formatter.format(result: searchResult)
        XCTAssertEqual(expectedOutput, output, "colorized match longer than maxLineLength searchResult matches expected")
    }

    func testSearchResult2MatchLongerColorize() {
        let pattern = "\\d+maxlen\\d+"
        let file = "./maxlen.txt"
        let fileResult = FileResult(filePath: file, fileType: FileType.text, fileSize: 0)
        let lineNum = 1
        let matchStartIndex = 11
        let matchEndIndex = 120
        let line = "ABCDEFGHIJ0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789ABCDEFGHIJ"
        let linesBeforeAfter: [String] = []
        let searchResult = SearchResult(
            searchPattern: pattern,
            file: fileResult,
            lineNum: lineNum,
            matchStartIndex: matchStartIndex,
            matchEndIndex: matchEndIndex,
            line: line,
            linesBefore: linesBeforeAfter,
            linesAfter: linesBeforeAfter
        )
        let expectedLine = "..." +
            ConsoleColor.GREEN +
            "3456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456" +
            ConsoleColor.RESET +
            "..."
        let expectedOutput = "\(file): \(lineNum): [\(matchStartIndex):\(matchEndIndex)]: \(expectedLine)"

        let settings = SearchSettings()
        settings.colorize = true
        settings.maxLineLength = 100
        let formatter = SearchResultFormatter(settings: settings)

        let output = formatter.format(result: searchResult)
        XCTAssertEqual(expectedOutput, output, "colorized match 2 longer than maxLineLength searchResult matches expected")
    }

    func testBinaryFileSearchResult() {
        let pattern = "Search"
        let file = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe"
        let fileResult = FileResult(filePath: file, fileType: FileType.binary, fileSize: 0)
        let lineNum = 0
        let matchStartIndex = 0
        let matchEndIndex = 0
        let searchResult = SearchResult(
            searchPattern: pattern,
            file: fileResult,
            lineNum: lineNum,
            matchStartIndex: matchStartIndex,
            matchEndIndex: matchEndIndex,
            line: "",
            linesBefore: [],
            linesAfter: []
        )
        let expectedOutput = "\(file) matches at [0:0]"

        let settings = SearchSettings()
        let formatter = SearchResultFormatter(settings: settings)

        let output = formatter.format(result: searchResult)
        XCTAssertEqual(expectedOutput, output, "binary searchResult matches expected")
    }

    func testMultiLineSearchResult() {
        let pattern = "Search"
        let file = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        let fileResult = FileResult(filePath: file, fileType: FileType.text, fileSize: 0)
        let lineNum = 10
        let matchStartIndex = 15
        let matchEndIndex = 23
        let line = "\tpublic class Searcher"
        let linesBefore = ["namespace CsSearch", "{"]
        let linesAfter = ["\t{", "\t\tprivate readonly FileTypes _fileTypes;"]
        let searchResult = SearchResult(
            searchPattern: pattern,
            file: fileResult,
            lineNum: lineNum,
            matchStartIndex: matchStartIndex,
            matchEndIndex: matchEndIndex,
            line: line,
            linesBefore: linesBefore,
            linesAfter: linesAfter
        )
        let expectedOutput =
            """
            ================================================================================
            \(file): \(lineNum): [\(matchStartIndex):\(matchEndIndex)]
            --------------------------------------------------------------------------------
               8 | namespace CsSearch
               9 | {
            > 10 | 	public class Searcher
              11 | 	{
              12 | 		private readonly FileTypes _fileTypes;

            """

        let settings = SearchSettings()
        settings.colorize = false
        let formatter = SearchResultFormatter(settings: settings)

        let output = formatter.format(result: searchResult)
        XCTAssertEqual(expectedOutput, output, "multiline searchResult matches expected")
    }
}
