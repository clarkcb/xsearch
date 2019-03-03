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

open class SearchSettings: CustomStringConvertible {
    var archivesOnly: Bool = DefaultSettings.archivesOnly
    var debug: Bool = DefaultSettings.debug
    var excludeHidden: Bool = DefaultSettings.excludeHidden
    var firstMatch: Bool = DefaultSettings.firstMatch
    var linesAfter = DefaultSettings.linesAfter
    var linesBefore = DefaultSettings.linesBefore
    var listDirs: Bool = DefaultSettings.listDirs
    var listFiles: Bool = DefaultSettings.listFiles
    var listLines: Bool = DefaultSettings.listLines
    var maxLineLength = DefaultSettings.maxLineLength
    var multiLineSearch: Bool = DefaultSettings.multiLineSearch
    var printResults: Bool = DefaultSettings.printResults
    var printUsage: Bool = DefaultSettings.printUsage
    var printVersion: Bool = DefaultSettings.printVersion
    var recursive: Bool = DefaultSettings.recursive
    var searchArchives: Bool = DefaultSettings.searchArchives
    var startPath = DefaultSettings.startPath
    var uniqueLines: Bool = DefaultSettings.uniqueLines
    var verbose: Bool = DefaultSettings.verbose

    var inArchiveExtensions = Set<String>()
    var inArchiveFilePatterns = Array<Regex>()
    var inDirPatterns = Array<Regex>()
    var inExtensions = Set<String>()
    var inFilePatterns = Array<Regex>()
    var inFileTypes = Array<FileType>()
    var inLinesAfterPatterns = Array<Regex>()
    var inLinesBeforePatterns = Array<Regex>()
    var linesAfterToPatterns = Array<Regex>()
    var linesAfterUntilPatterns = Array<Regex>()
    var outArchiveExtensions = Set<String>()
    var outArchiveFilePatterns = Array<Regex>()
    var outDirPatterns = Array<Regex>()
    var outExtensions = Set<String>()
    var outFilePatterns = Array<Regex>()
    var outFileTypes = Array<FileType>()
    var outLinesAfterPatterns = Array<Regex>()
    var outLinesBeforePatterns = Array<Regex>()
    var searchPatterns = Array<Regex>()

    fileprivate func splitExtensions(_ exts: String) -> [String] {
        return exts.split {$0 == ","}.map { String($0) }
    }

    func addInArchiveExtension(_ ext: String) {
        for x in splitExtensions(ext) {
            inArchiveExtensions.insert(x)
        }
    }

    func addInExtension(_ ext: String) {
        for x in splitExtensions(ext) {
            inExtensions.insert(x)
        }
    }

    func addOutArchiveExtension(_ ext: String) {
        for x in splitExtensions(ext) {
            outArchiveExtensions.insert(x)
        }
    }

    func addOutExtension(_ ext: String) {
        for x in splitExtensions(ext) {
            outExtensions.insert(x)
        }
    }

    open var description: String {
        var s = "SearchSettings("
        s += "archivesOnly=\(archivesOnly)"
        s += ", debug=\(debug)"
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
        s += ", startPath=\"\(startPath!)\""
        s += ", uniqueLines=\(uniqueLines)"
        s += ", verbose=\(verbose)"
        s += ")"
        return s
    }
}
