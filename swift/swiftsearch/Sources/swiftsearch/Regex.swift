//
//  Regex.swift
//  swiftsearch
//
// from http://benscheirman.com/2014/06/regex-in-swift/ with modifications
//

import Foundation

public struct Regex {
    let pattern: String
    let expression: NSRegularExpression

    public init(_ pattern: String) {
        self.pattern = pattern
        // var error: NSError?
        expression = try! NSRegularExpression(pattern: pattern,
                                              options: .dotMatchesLineSeparators)
    }

    private func strRange(_ str: String) -> NSRange {
        NSMakeRange(0, str.lengthOfBytes(using: String.Encoding.isoLatin1))
    }

    public func matches(_ str: String) -> [NSTextCheckingResult] {
        expression.matches(in: str, options: [], range: strRange(str))
    }

    public func firstMatch(_ str: String) -> NSTextCheckingResult? {
        expression.firstMatch(in: str, options: [], range: strRange(str))
    }

    public func test(_ str: String) -> Bool {
        expression.numberOfMatches(in: str, options: [], range: strRange(str)) > 0
    }
}
