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
    static let colorize = true
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
    var colorize: Bool = DefaultSettings.colorize
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
        let s = "SearchSettings(" +
            "archivesOnly=\(archivesOnly)" +
            ", colorize=\(colorize)" +
            ", debug=\(debug)" +
            ", excludeHidden=\(excludeHidden)" +
            ", firstMatch=\(firstMatch)" +
            ", inArchiveExtensions=\(setToString(inArchiveExtensions))" +
            ", inArchiveExtensions=\(setToString(inArchiveExtensions))" +
            ", inDirPatterns=\(arrayToString(inDirPatterns))" +
            ", inExtensions=\(setToString(inExtensions))" +
            ", inFilePatterns=\(arrayToString(inFilePatterns))" +
            ", inFileTypes=\(arrayToString(inFileTypes.map { FileTypes.toName($0) }))" +
            ", inLinesAfterPatterns=\(arrayToString(inLinesAfterPatterns))" +
            ", inLinesBeforePatterns=\(arrayToString(inLinesBeforePatterns))" +
            ", linesAfterToPatterns=\(arrayToString(linesAfterToPatterns))" +
            ", linesAfterUntilPatterns=\(arrayToString(linesAfterUntilPatterns))" +
            ", linesAfter=\(linesAfter)" +
            ", linesBefore=\(linesBefore)" +
            ", listDirs=\(listDirs)" +
            ", listFiles=\(listFiles)" +
            ", listLines=\(listLines)" +
            ", maxLineLength=\(maxLineLength)" +
            ", outArchiveExtensions=\(setToString(outArchiveExtensions))" +
            ", outArchiveExtensions=\(setToString(outArchiveExtensions))" +
            ", outDirPatterns=\(arrayToString(outDirPatterns))" +
            ", outExtensions=\(setToString(outExtensions))" +
            ", outFilePatterns=\(arrayToString(outFilePatterns))" +
            ", outFileTypes=\(arrayToString(outFileTypes.map { FileTypes.toName($0) }))" +
            ", outLinesAfterPatterns=\(arrayToString(outLinesAfterPatterns))" +
            ", outLinesBeforePatterns=\(arrayToString(outLinesBeforePatterns))" +
            ", printResults=\(printResults)" +
            ", printUsage=\(printUsage)" +
            ", printVersion=\(printVersion)" +
            ", recursive=\(recursive)" +
            ", searchArchives=\(searchArchives)" +
            ", searchPatterns=\(arrayToString(searchPatterns))" +
            ", startPath=\"\(startPath!)\"" +
            ", textFileEncoding=\"\(textFileEncoding)\"" +
            ", uniqueLines=\(uniqueLines)" +
            ", verbose=\(verbose)" +
            ")"
        return s
    }
}
