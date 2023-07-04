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
        let settings = SearchSettings()
        settings.colorize = false
        let formatter = SearchResultFormatter(settings: settings)
        let pattern = "Search"
        let filepath = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        let fileResult = FileResult(filePath: filepath, fileType: FileType.code)
        let lineNum = 10
        let matchStartIndex = 15
        let matchEndIndex = 23
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
        let output = formatter.format(result: searchResult)
        XCTAssertEqual(expectedOutput, output, "single-line searchResult matches expected")
    }

    func testSingleLineLongerThanMaxLineLengthSearchResult() {
        let settings = SearchSettings()
        settings.colorize = false
        settings.maxLineLength = 100
        let formatter = SearchResultFormatter(settings: settings)
        let pattern = "maxlen"
        let file = "./maxlen.txt"
        let fileResult = FileResult(filePath: file, fileType: FileType.text)
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
        let output = formatter.format(result: searchResult)
        XCTAssertEqual(expectedOutput, output, "single-line longer than maxLineLength searchResult matches expected")
    }

    func testSingleLineLongerColorizeSearchResult() {
        let settings = SearchSettings()
        settings.colorize = true
        settings.maxLineLength = 100
        let formatter = SearchResultFormatter(settings: settings)
        let pattern = "maxlen"
        let file = "./maxlen.txt"
        let fileResult = FileResult(filePath: file, fileType: FileType.text)
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
            Color.GREEN +
            "maxlen" +
            Color.RESET +
            "89012345678901234567890123456789012345678901..."
        let expectedOutput = "\(file): \(lineNum): [\(matchStartIndex):\(matchEndIndex)]: \(expectedLine)"
        let output = formatter.format(result: searchResult)
        XCTAssertEqual(expectedOutput, output, "single-line colorized searchResult matches expected")
    }

    func testBinaryFileSearchResult() {
        let settings = SearchSettings()
        let formatter = SearchResultFormatter(settings: settings)
        let pattern = "Search"
        let file = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe"
        let fileResult = FileResult(filePath: file, fileType: FileType.binary)
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
        let output = formatter.format(result: searchResult)
        XCTAssertEqual(expectedOutput, output, "binary searchResult matches expected")
    }

    func testMultiLineSearchResult() {
        let settings = SearchSettings()
        settings.colorize = false
        let formatter = SearchResultFormatter(settings: settings)
        let pattern = "Search"
        let file = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        let fileResult = FileResult(filePath: file, fileType: FileType.text)
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
        let output = formatter.format(result: searchResult)
        XCTAssertEqual(expectedOutput, output, "multiline searchResult matches expected")
    }
}
