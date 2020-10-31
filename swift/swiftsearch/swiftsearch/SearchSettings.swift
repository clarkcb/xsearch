//
//  SearchSettings.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

// import Foundation

enum DefaultSettings {
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
    static let textFileEncoding: String = "UTF-8"
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
    var textFileEncoding = DefaultSettings.textFileEncoding
    var uniqueLines: Bool = DefaultSettings.uniqueLines
    var verbose: Bool = DefaultSettings.verbose

    var inArchiveExtensions = Set<String>()
    var inArchiveFilePatterns = [Regex]()
    var inDirPatterns = [Regex]()
    var inExtensions = Set<String>()
    var inFilePatterns = [Regex]()
    var inFileTypes = [FileType]()
    var inLinesAfterPatterns = [Regex]()
    var inLinesBeforePatterns = [Regex]()
    var linesAfterToPatterns = [Regex]()
    var linesAfterUntilPatterns = [Regex]()
    var outArchiveExtensions = Set<String>()
    var outArchiveFilePatterns = [Regex]()
    var outDirPatterns = [Regex]()
    var outExtensions = Set<String>()
    var outFilePatterns = [Regex]()
    var outFileTypes = [FileType]()
    var outLinesAfterPatterns = [Regex]()
    var outLinesBeforePatterns = [Regex]()
    var searchPatterns = [Regex]()

    fileprivate func splitExtensions(_ exts: String) -> [String] {
        exts.split { $0 == "," }.map { String($0) }
    }

    func addInArchiveExtension(_ exts: String) {
        for ext in splitExtensions(exts) {
            inArchiveExtensions.insert(ext)
        }
    }

    func addInExtension(_ exts: String) {
        for ext in splitExtensions(exts) {
            inExtensions.insert(ext)
        }
    }

    func addOutArchiveExtension(_ exts: String) {
        for ext in splitExtensions(exts) {
            outArchiveExtensions.insert(ext)
        }
    }

    func addOutExtension(_ exts: String) {
        for ext in splitExtensions(exts) {
            outExtensions.insert(ext)
        }
    }

    func setArchivesOnly(_ archivesOnly: Bool) {
        self.archivesOnly = archivesOnly
        if archivesOnly { searchArchives = true }
    }

    func setDebug(_ debug: Bool) {
        self.debug = debug
        if debug { verbose = true }
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
