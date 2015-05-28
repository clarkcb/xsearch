//
//  SearchSettings.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

//import Foundation

class DefaultSettings {
    static let archivesOnly = false
    static let debug = false
    static let doTiming = false
    static let excludeHidden = true
    static let firstMatch = false
    static let linesAfter = 0
    static let linesBefore = 0
    static let listDirs = false
    static let listFiles = false
    static let listLines = false
    static let maxLineLength = 150
    static let multiLineSearch = false
    static let printResults = true
    static let printUsage = false
    static let printVersion = false
    static let recursive = true
    static let searchArchives = false
    static let startPath: String? = ""
    static let uniqueLines = false
    static let verbose = false
}

public class SearchSettings: Printable {
    var archivesOnly = DefaultSettings.archivesOnly
    var debug = DefaultSettings.debug
    var doTiming = DefaultSettings.doTiming
    var excludeHidden = DefaultSettings.excludeHidden
    var firstMatch = DefaultSettings.firstMatch
    var linesAfter = DefaultSettings.linesAfter
    var linesBefore = DefaultSettings.linesBefore
    var listDirs = DefaultSettings.listDirs
    var listFiles = DefaultSettings.listFiles
    var listLines = DefaultSettings.listLines
    var maxLineLength = DefaultSettings.maxLineLength
    var multiLineSearch = DefaultSettings.multiLineSearch
    var printResults = DefaultSettings.printResults
    var printUsage = DefaultSettings.printUsage
    var printVersion = DefaultSettings.printVersion
    var recursive = DefaultSettings.recursive
    var searchArchives = DefaultSettings.searchArchives
    var startPath = DefaultSettings.startPath
    var uniqueLines = DefaultSettings.uniqueLines
    var verbose = DefaultSettings.verbose

    var inArchiveExtensions = Set<String>()
    var inArchiveFilePatterns = Array<Regex>()
    var inDirPatterns = Array<Regex>()
    var inExtensions = Set<String>()
    var inFilePatterns = Array<Regex>()
    var inLinesAfterPatterns = Array<Regex>()
    var inLinesBeforePatterns = Array<Regex>()
    var linesAfterToPatterns = Array<Regex>()
    var linesAfterUntilPatterns = Array<Regex>()
    var outArchiveExtensions = Set<String>()
    var outArchiveFilePatterns = Array<Regex>()
    var outDirPatterns = Array<Regex>()
    var outExtensions = Set<String>()
    var outFilePatterns = Array<Regex>()
    var outLinesAfterPatterns = Array<Regex>()
    var outLinesBeforePatterns = Array<Regex>()
    var searchPatterns = Array<Regex>()

    private func splitExtensions(exts: String) -> [String] {
        return split(exts) {$0 == ","}
    }

    func addInArchiveExtension(ext: String) {
        for x in splitExtensions(ext) {
            inArchiveExtensions.insert(x)
        }
    }

    func addInExtension(ext: String) {
        for x in splitExtensions(ext) {
            inExtensions.insert(x)
        }
    }

    func addOutArchiveExtension(ext: String) {
        for x in splitExtensions(ext) {
            outArchiveExtensions.insert(x)
        }
    }

    func addOutExtension(ext: String) {
        for x in splitExtensions(ext) {
            outExtensions.insert(x)
        }
    }

    public var description: String {
        var s = "SearchSettings("
        s += "archivesOnly=\(archivesOnly)"
        s += ", debug=\(debug)"
        s += ", doTiming=\(doTiming)"
        s += ", excludeHidden=\(excludeHidden)"
        s += ", firstMatch=\(firstMatch)"
        s += ", inArchiveExtensions=\(setToString(inArchiveExtensions))"
        s += ", inArchiveExtensions=\(setToString(inArchiveExtensions))"
        s += ", inDirPatterns=\(arrayToString(inDirPatterns))"
        s += ", inExtensions=\(setToString(inExtensions))"
        s += ", inFilePatterns=\(arrayToString(inFilePatterns))"
        s += ", inLinesAfterPatterns=\(arrayToString(inLinesAfterPatterns))"
        s += ", inLinesBeforePatterns=\(arrayToString(inLinesBeforePatterns))"
        s += ", linesAfterToPatterns=\(arrayToString(linesAfterToPatterns))"
        s += ", linesAfterUntilPatterns=\(arrayToString(linesAfterUntilPatterns))"
        s += ", linesAfter=\(linesAfter)"
        s += ", linesBefore=\(linesBefore)"
        s += ", listDirs=\(listDirs)"
        s += ", listFiles=\(listFiles)"
        s += ", listLines=\(listLines)"
        s += ", maxLineLength=\(maxLineLength)"
        s += ", outArchiveExtensions=\(setToString(outArchiveExtensions))"
        s += ", outArchiveExtensions=\(setToString(outArchiveExtensions))"
        s += ", outDirPatterns=\(arrayToString(outDirPatterns))"
        s += ", outExtensions=\(setToString(outExtensions))"
        s += ", outFilePatterns=\(arrayToString(outFilePatterns))"
        s += ", outLinesAfterPatterns=\(arrayToString(outLinesAfterPatterns))"
        s += ", outLinesBeforePatterns=\(arrayToString(outLinesBeforePatterns))"
        s += ", printResults=\(printResults)"
        s += ", printUsage=\(printUsage)"
        s += ", printVersion=\(printVersion)"
        s += ", recursive=\(recursive)"
        s += ", searchArchives=\(searchArchives)"
        s += ", searchPatterns=\(arrayToString(searchPatterns))"
        s += ", startPath=\"\(startPath)\""
        s += ", uniqueLines=\(uniqueLines)"
        s += ", verbose=\(verbose)"
        s += ")"
        return s
    }
}
