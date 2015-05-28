//
//  SearchResult.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/20/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public class SearchResult: Printable {
    let searchPattern: String
    var file: String?
    let lineNum: Int
    let matchStartIndex: Int
    let matchEndIndex: Int
    let line: String
    let linesBefore: [String]
    let linesAfter: [String]

    init(searchPattern: String, file: String, lineNum: Int,
        matchStartIndex: Int, matchEndIndex: Int, line: String,
        linesBefore: [String], linesAfter: [String]) {
            self.searchPattern = searchPattern
            self.file = file
            self.lineNum = lineNum
            self.matchStartIndex = matchStartIndex
            self.matchEndIndex = matchEndIndex
            self.line = line
            self.linesBefore = linesBefore
            self.linesAfter = linesAfter
    }

    public var description: String {
        if !linesBefore.isEmpty || !linesAfter.isEmpty {
            return multiLineToString()
        } else {
            return singleLineToString()
        }
    }

    // a string to sort the result by
    func sortString() -> String {
        var s = ""
        if file != nil {
            var pathCount: Int = FileUtil.splitPath(file!).count
            s += String(format: "%02d:", pathCount)
            s += file!.lowercaseString
        } else {
            s += "<text>"
        }
        s += String(format: ":%05d", lineNum)
        s += String(format: ":[%05d:%05d]", matchStartIndex, matchEndIndex)
        return s
    }

    private func singleLineToString() -> String {
        var s = file ?? "<text>"
        if lineNum > 0 {
            s += ": \(lineNum): [\(matchStartIndex):\(matchEndIndex)]: "
            s += line.stringByTrimmingCharactersInSet(whitespace)
        } else {
            s += " matches at [\(matchStartIndex):\(matchEndIndex)]"
        }
        return s
    }

    private func linesToString(lines: [String], startNum: Int, format: String) -> String {
        var s = ""
        var currentLineNum = startNum
        for line in lines {
            var currentLine = "  " + String(format: format, currentLineNum)
            currentLine += " | \(line)\n"
            s += currentLine
            ++currentLineNum
        }
        return s
    }

    private func multiLineToString() -> String {
        let sepLen = 80
        var s = ""
        let filepath = file ?? "<text>"
        let eq = "="
        let dash = "-"
        s += "\(eq.repeat(sepLen))\n"
        s += "\(filepath): \(lineNum): [\(matchStartIndex):\(matchEndIndex)]\n"
        s += "\(dash.repeat(sepLen))\n"

        let maxLineNum = lineNum + linesAfter.count
        let maxNumLen = count("\(maxLineNum)")
        let format = "%\(maxNumLen)d"
        if linesBefore.count > 0 {
            s += linesToString(linesBefore, startNum: lineNum - linesBefore.count, format: format)
        }
        var currentLine = String(format: format, lineNum)
        currentLine = "> \(currentLine) | \(line)\n"
        s += currentLine
        if linesAfter.count > 0 {
            s += linesToString(linesAfter, startNum: lineNum + 1, format: format)
        }
        return s
    }
}
