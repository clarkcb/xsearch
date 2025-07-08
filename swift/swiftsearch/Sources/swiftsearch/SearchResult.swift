//
//  SearchResult.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/20/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation
import swiftfind

public struct SearchResult {
    public let searchPattern: String
    public var file: FileResult?
    public let lineNum: Int
    public let matchStartIndex: Int
    public let matchEndIndex: Int
    public let line: String
    public let linesBefore: [String]
    public let linesAfter: [String]

    public init(searchPattern: String, file: FileResult?, lineNum: Int,
                matchStartIndex: Int, matchEndIndex: Int, line: String,
                linesBefore: [String], linesAfter: [String])
    {
        self.searchPattern = searchPattern
        self.file = file
        self.lineNum = lineNum
        self.matchStartIndex = matchStartIndex
        self.matchEndIndex = matchEndIndex
        self.line = line
        self.linesBefore = linesBefore
        self.linesAfter = linesAfter
    }
}

public class SearchResultFormatter {
    let settings: SearchSettings
    let fileFormatter: FileResultFormatter
    public var formatLine = { (line: String) in line }

    public init(settings: SearchSettings) {
        self.settings = settings
        self.fileFormatter = FileResultFormatter(settings: settings)
        if settings.colorize {
            formatLine = formatLineWithColor
        }
    }

    private func formatLineWithColor(_ line: String) -> String {
        var formattedLine = line
        for p in settings.searchPatterns {
            let match = p.firstMatch(formattedLine)
            if match != nil {
                formattedLine = colorize(formattedLine, match!.range.lowerBound, match!.range.upperBound)
                break
            }
        }
        return formattedLine
    }

    public func format(result: SearchResult) -> String {
        if !result.linesBefore.isEmpty || !result.linesAfter.isEmpty {
            return multiLineFormat(result: result)
        } else {
            return singleLineFormat(result: result)
        }
    }

    private func colorize(_ s: String, _ matchStartIndex: Int, _ matchEndIndex: Int) -> String {
        fileFormatter.colorize(s, matchStartIndex, matchEndIndex)
    }

    private func formatMatchingLine(result: SearchResult) -> String {
        let whitespaceChars: Set<Character> = [" ", "\t", "\n", "\r"]
        let leadingWhitespaceCount = result.line.prefix(while: { whitespaceChars.contains($0) }).count
        var formatted = result.line.trimmingCharacters(in: whitespace as CharacterSet)
        var formattedLength = formatted.count
        let maxLineEndIndex = formattedLength - 1
        let matchLength = result.matchEndIndex - result.matchStartIndex
        var matchStartIndex = result.matchStartIndex - 1 - leadingWhitespaceCount
        var matchEndIndex = matchStartIndex + matchLength

        if formattedLength > settings.maxLineLength {
            var lineStartIndex = matchStartIndex
            var lineEndIndex = lineStartIndex + matchLength
            matchStartIndex = 0
            matchEndIndex = matchLength

            while lineEndIndex > formattedLength - 1 {
                lineStartIndex -= 1
                lineEndIndex -= 1
                matchStartIndex += 1
                matchEndIndex += 1
            }

            formattedLength = lineEndIndex - lineStartIndex
            while formattedLength < settings.maxLineLength {
                if lineStartIndex > 0 {
                    lineStartIndex -= 1
                    matchStartIndex += 1
                    matchEndIndex += 1
                    formattedLength = lineEndIndex - lineStartIndex
                }
                if formattedLength < settings.maxLineLength, lineEndIndex < maxLineEndIndex {
                    lineEndIndex += 1
                }
                formattedLength = lineEndIndex - lineStartIndex
            }

            var before = ""
            var after = ""
            if lineStartIndex > 2 {
                before = "..."
                lineStartIndex += 3
            }
            if lineEndIndex < maxLineEndIndex - 3 {
                after = "..."
                lineEndIndex -= 3
            }
            let strLineStartIndex = formatted.index(formatted.startIndex, offsetBy: lineStartIndex)
            let strLineEndIndex = formatted.index(formatted.startIndex, offsetBy: lineEndIndex)

            formatted = before + String(formatted[strLineStartIndex ..< strLineEndIndex]) + after
        }

        if settings.colorize {
            formatted = colorize(formatted, matchStartIndex, matchEndIndex)
        }

        return formatted
    }

    private func formatFilePath(result: SearchResult) -> String {
        result.file == nil ? "<text>" : fileFormatter.formatFileResult(result.file!)
    }

    private func singleLineFormat(result: SearchResult) -> String {
        var str = formatFilePath(result: result)
        if result.lineNum > 0 {
            str += ": \(result.lineNum): [\(result.matchStartIndex):\(result.matchEndIndex)]: "
            str += formatMatchingLine(result: result)
        } else {
            str += " matches at [\(result.matchStartIndex):\(result.matchEndIndex)]"
        }
        return str
    }

    private func linesToString(_ lines: [String], startNum: Int, format: String) -> String {
        var str = ""
        var currentLineNum = startNum
        for line in lines {
            var currentLine = "  " + String(format: format, currentLineNum)
            currentLine += " | \(line)\n"
            str += currentLine
            currentLineNum += 1
        }
        return str
    }

    private func multiLineFormat(result: SearchResult) -> String {
        let sepLen = 80
        var str = ""
        let filepath = formatFilePath(result: result)
        let eq = "="
        let dash = "-"
        str += "\(eq.repeat(sepLen))\n"
        str += "\(filepath): \(result.lineNum): [\(result.matchStartIndex):\(result.matchEndIndex)]\n"
        str += "\(dash.repeat(sepLen))\n"

        let maxLineNum = result.lineNum + result.linesAfter.count
        let maxNumLen = "\(maxLineNum)".lengthOfBytes(using: String.Encoding.utf8)
        let format = "%\(maxNumLen)d"
        if result.linesBefore.count > 0 {
            str += linesToString(result.linesBefore, startNum: result.lineNum - result.linesBefore.count, format: format)
        }
        var line = result.line
        if settings.colorize {
            line = colorize(line, result.matchStartIndex - 1, result.matchEndIndex - 1)
        }
        var currentLine = String(format: format, result.lineNum)
        currentLine = "> \(currentLine) | \(line)\n"
        str += currentLine
        if result.linesAfter.count > 0 {
            str += linesToString(result.linesAfter, startNum: result.lineNum + 1, format: format)
        }
        return str
    }
}

public class SearchResultSorter {
    let settings: SearchSettings
    let fileResultSorter: FileResultSorter
    
    public init(settings: SearchSettings) {
        self.settings = settings
        self.fileResultSorter = FileResultSorter(settings: settings)
    }

    public func cmpBySearchFields(_ sr1: SearchResult, _ sr2: SearchResult) -> Int {
        if sr1.lineNum == sr2.lineNum {
            if sr1.matchStartIndex == sr2.matchStartIndex {
                if sr1.matchEndIndex == sr2.matchEndIndex {
                    return 0
                }
                return sr1.matchEndIndex < sr2.matchEndIndex ? -1 : 1
            }
            return sr1.matchStartIndex < sr2.matchStartIndex ? -1 : 1
        }
        return sr1.lineNum < sr2.lineNum ? -1 : 1
    }

    public func cmpByFilePath(_ sr1: SearchResult, _ sr2: SearchResult) -> Int {
        if sr1.file != nil && sr2.file != nil {
            let fileCmp = fileResultSorter.cmpByFilePath(sr1.file!, sr2.file!)
            if fileCmp != 0 {
                return fileCmp
            }
        }
        return cmpBySearchFields(sr1, sr2)
    }

    public func sortByFilePath(_ sr1: SearchResult, _ sr2: SearchResult) -> Bool {
        return cmpByFilePath(sr1, sr2) <= 0
    }

    public func cmpByFileName(_ sr1: SearchResult, _ sr2: SearchResult) -> Int {
        if sr1.file != nil && sr2.file != nil {
            let fileCmp = fileResultSorter.cmpByFileName(sr1.file!, sr2.file!)
            if fileCmp != 0 {
                return fileCmp
            }
        }
        return cmpBySearchFields(sr1, sr2)
    }

    public func sortByFileName(_ sr1: SearchResult, _ sr2: SearchResult) -> Bool {
        return cmpByFileName(sr1, sr2) <= 0
    }

    public func cmpByFileSize(_ sr1: SearchResult, _ sr2: SearchResult) -> Int {
        if sr1.file != nil && sr2.file != nil {
            let fileCmp = fileResultSorter.cmpByFileSize(sr1.file!, sr2.file!)
            if fileCmp != 0 {
                return fileCmp
            }
        }
        return cmpBySearchFields(sr1, sr2)
    }

    public func sortByFileSize(_ sr1: SearchResult, _ sr2: SearchResult) -> Bool {
        return cmpByFileSize(sr1, sr2) <= 0
    }

    public func cmpByFileType(_ sr1: SearchResult, _ sr2: SearchResult) -> Int {
        if sr1.file != nil && sr2.file != nil {
            let fileCmp = fileResultSorter.cmpByFileType(sr1.file!, sr2.file!)
            if fileCmp != 0 {
                return fileCmp
            }
        }
        return cmpBySearchFields(sr1, sr2)
    }

    public func sortByFileType(_ sr1: SearchResult, _ sr2: SearchResult) -> Bool {
        return cmpByFileType(sr1, sr2) <= 0
    }

    public func cmpByLastMod(_ sr1: SearchResult, _ sr2: SearchResult) -> Int {
        if sr1.file != nil && sr2.file != nil {
            let fileCmp = fileResultSorter.cmpByLastMod(sr1.file!, sr2.file!)
            if fileCmp != 0 {
                return fileCmp
            }
        }
        return cmpBySearchFields(sr1, sr2)
    }

    public func sortByLastMod(_ sr1: SearchResult, _ sr2: SearchResult) -> Bool {
        return cmpByLastMod(sr1, sr2) <= 0
    }

    private func getSearchResultComparator() -> (SearchResult, SearchResult) -> Bool {
        if settings.sortDescending {
            switch settings.sortBy {
            case SortBy.fileName:
                return { (sr1: SearchResult, sr2: SearchResult) -> Bool in return self.sortByFileName(sr2, sr1) }
            case SortBy.fileSize:
                return { (sr1: SearchResult, sr2: SearchResult) -> Bool in return self.sortByFileSize(sr2, sr1) }
            case SortBy.fileType:
                return { (sr1: SearchResult, sr2: SearchResult) -> Bool in return self.sortByFileType(sr2, sr1) }
            case SortBy.lastMod:
                return { (sr1: SearchResult, sr2: SearchResult) -> Bool in return self.sortByLastMod(sr2, sr1) }
            default:
                return { (sr1: SearchResult, sr2: SearchResult) -> Bool in return self.sortByFilePath(sr2, sr1) }
            }
        } else {
            switch settings.sortBy {
            case SortBy.fileName:
                return sortByFileName
            case SortBy.fileSize:
                return sortByFileSize
            case SortBy.fileType:
                return sortByFileType
            case SortBy.lastMod:
                return sortByLastMod
            default:
                return sortByFilePath
            }
        }
    }

    public func sort(_ searchResults: [SearchResult]) -> [SearchResult] {
        let searchResultComparator = getSearchResultComparator()
        return searchResults.sorted(by: searchResultComparator)
    }
}
