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

    public init(settings: SearchSettings) {
        self.settings = settings
    }

    public func format(result: SearchResult) -> String {
        if !result.linesBefore.isEmpty || !result.linesAfter.isEmpty {
            return multiLineFormat(result: result)
        } else {
            return singleLineFormat(result: result)
        }
    }

    private func colorize(_ s: String, _ matchStartIndex: Int, _ matchEndIndex: Int) -> String {
        let strMatchStartIndex = s.index(s.startIndex, offsetBy: matchStartIndex)
        let strMatchEndIndex = s.index(s.startIndex, offsetBy: matchEndIndex)
        return String(s[..<strMatchStartIndex]) +
            Color.GREEN +
            String(s[strMatchStartIndex ..< strMatchEndIndex]) +
            Color.RESET +
            String(s[strMatchEndIndex...])
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
        result.file == nil ? "<text>" : FileUtil.formatPath(result.file!.filePath,
                                                            forPaths: Array(settings.paths))
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
