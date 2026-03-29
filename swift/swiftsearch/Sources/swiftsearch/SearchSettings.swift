//
//  SearchSettings.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import swiftfind

public enum DefaultSearchSettings {
    public static let firstMatch = false
    public static let lineColor = Color.green
    public static let linesAfter: Int32 = 0
    public static let linesBefore: Int32 = 0
    public static let maxLineLength: Int32 = 150
    public static let multiLineSearch = false
    public static let printLines = false
    public static let printMatches = false
    public static let printResults = true
    public static let searchArchives = false
    public static let textFileEncoding: String = "UTF-8"
    public static let uniqueLines = false
}

public class SearchSettings: FindSettings {
    public var firstMatch: Bool = DefaultSearchSettings.firstMatch
    public var lineColor: Color = DefaultSearchSettings.lineColor
    public var linesAfter: Int32 = DefaultSearchSettings.linesAfter
    public var linesBefore: Int32 = DefaultSearchSettings.linesBefore
    public var maxLineLength: Int32 = DefaultSearchSettings.maxLineLength
    public var multiLineSearch: Bool = DefaultSearchSettings.multiLineSearch
    public var printLines: Bool = DefaultSearchSettings.printLines
    public var printMatches: Bool = DefaultSearchSettings.printMatches
    public var printResults: Bool = DefaultSearchSettings.printResults
    public var searchArchives: Bool = DefaultSearchSettings.searchArchives
    public var textFileEncoding: String = DefaultSearchSettings.textFileEncoding
    public var uniqueLines: Bool = DefaultSearchSettings.uniqueLines

    public var inLinesAfterPatterns = [Regex]()
    public var inLinesBeforePatterns = [Regex]()
    public var linesAfterToPatterns = [Regex]()
    public var linesAfterUntilPatterns = [Regex]()
    public var outLinesAfterPatterns = [Regex]()
    public var outLinesBeforePatterns = [Regex]()
    public var searchPatterns = [Regex]()


    override public var archivesOnly: Bool {
        get {
            _archivesOnly
        }
        set {
            _archivesOnly = newValue
            if newValue {
                includeArchives = newValue
                searchArchives = newValue
            }
        }
    }

    public func addInLinesAfterPattern(_ pattern: String) {
        inLinesAfterPatterns.append(Regex(pattern))
    }

    public func addInLinesBeforePattern(_ pattern: String) {
        inLinesBeforePatterns.append(Regex(pattern))
    }

    public func addLinesAfterToPattern(_ pattern: String) {
        linesAfterToPatterns.append(Regex(pattern))
    }

    public func addLinesAfterUntilPattern(_ pattern: String) {
        linesAfterUntilPatterns.append(Regex(pattern))
    }

    public func addOutLinesAfterPattern(_ pattern: String) {
        outLinesAfterPatterns.append(Regex(pattern))
    }

    public func addOutLinesBeforePattern(_ pattern: String) {
        outLinesBeforePatterns.append(Regex(pattern))
    }

    public func addSearchPattern(_ pattern: String) {
        searchPatterns.append(Regex(pattern))
    }
}
