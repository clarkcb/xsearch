//
//  Regex.swift
//  swiftsearch
//
// from http://benscheirman.com/2014/06/regex-in-swift/ with modifications
//

import Foundation

public class Regex {
    let expression: NSRegularExpression
    let pattern: String

    public init(_ pattern: String) {
        self.pattern = pattern
        // var error: NSError?
        expression = try! NSRegularExpression(pattern: pattern,
                                              options: .dotMatchesLineSeparators)
    }

    public func matches(_ str: String) -> [NSTextCheckingResult] {
        expression.matches(in: str, options: [],
                           range: NSMakeRange(0, str.lengthOfBytes(using: String.Encoding.isoLatin1)))
    }

    public func test(_ str: String) -> Bool {
        matches(str).count > 0
    }
}
