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
        var error: NSError?
        self.expression = NSRegularExpression(pattern: pattern,
            options: .DotMatchesLineSeparators, error: &error)!
    }

    func matches(s: String) -> [NSTextCheckingResult] {
        return self.expression.matchesInString(s, options: nil,
            range: NSMakeRange(0, count(s))) as! Array<NSTextCheckingResult>
    }

    func test(s: String) -> Bool {
        return matches(s).count > 0
    }
}
