//
//  SearchResult.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/20/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

open class SearchResult: CustomStringConvertible {
    let searchPattern: String
    var file: SearchFile?
    let lineNum: Int
    let matchStartIndex: Int
    let matchEndIndex: Int
    let line: String
    let linesBefore: [String]
    let linesAfter: [String]

    init(searchPattern: String, file: SearchFile?, lineNum: Int,
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

    open var description: String {
        if !linesBefore.isEmpty || !linesAfter.isEmpty {
            return multiLineToString()
        } else {
            return singleLineToString()
        }
    }

    fileprivate func singleLineToString() -> String {
        var s = file?.description ?? "<text>"
        if lineNum > 0 {
            s += ": \(lineNum): [\(matchStartIndex):\(matchEndIndex)]: "
            s += line.trimmingCharacters(in: whitespace as CharacterSet)
        } else {
            s += " matches at [\(matchStartIndex):\(matchEndIndex)]"
        }
        return s
    }

    fileprivate func linesToString(_ lines: [String], startNum: Int, format: String) -> String {
        var s = ""
        var currentLineNum = startNum
        for line in lines {
            var currentLine = "  " + String(format: format, currentLineNum)
            currentLine += " | \(line)\n"
            s += currentLine
            currentLineNum += 1
        }
        return s
    }

    fileprivate func multiLineToString() -> String {
        let sepLen = 80
        var s = ""
        let filepath = file?.description ?? "<text>"
        let eq = "="
        let dash = "-"
        s += "\(eq.`repeat`(sepLen))\n"
        s += "\(filepath): \(lineNum): [\(matchStartIndex):\(matchEndIndex)]\n"
        s += "\(dash.`repeat`(sepLen))\n"

        let maxLineNum = lineNum + linesAfter.count
        let maxNumLen = "\(maxLineNum)".lengthOfBytes(using: String.Encoding.utf8)
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
