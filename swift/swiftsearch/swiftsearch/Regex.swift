//
//  Regex.swift
//  swiftsearch
//
// from http://benscheirman.com/2014/06/regex-in-swift/ with modifications
//

import Foundation

class Regex {
    let expression: NSRegularExpression
    let pattern: String

    init(_ pattern: String) {
        self.pattern = pattern
        //var error: NSError?
        self.expression = try! NSRegularExpression(pattern: pattern,
            options: .dotMatchesLineSeparators)
    }

    func matches(_ s: String) -> [NSTextCheckingResult] {
        return self.expression.matches(in: s, options: [],
            range: NSMakeRange(0, s.lengthOfBytes(using: String.Encoding.utf8)))
    }

    func test(_ s: String) -> Bool {
        return matches(s).count > 0
    }
}
