//
//  SearchSettings.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

// import Foundation

public enum DefaultSettings {
    public static let archivesOnly = false
    public static let colorize = true
    public static let debug = false
    public static let excludeHidden = true
    public static let firstMatch = false
    public static let linesAfter = 0
    public static let linesBefore = 0
    public static let listDirs = false
    public static let listFiles = false
    public static let listLines = false
    public static let maxLineLength = 150
    public static let multiLineSearch = false
    public static let printResults = true
    public static let printUsage = false
    public static let printVersion = false
    public static let recursive = true
    public static let searchArchives = false
    public static let startPath: String? = ""
    public static let textFileEncoding: String = "UTF-8"
    public static let uniqueLines = false
    public static let verbose = false
}

public class SearchSettings: CustomStringConvertible {
    private var _archivesOnly: Bool = DefaultSettings.archivesOnly
    public var colorize: Bool = DefaultSettings.colorize
    private var _debug: Bool = DefaultSettings.debug
    public var excludeHidden: Bool = DefaultSettings.excludeHidden
    public var firstMatch: Bool = DefaultSettings.firstMatch
    public var linesAfter = DefaultSettings.linesAfter
    public var linesBefore = DefaultSettings.linesBefore
    public var listDirs: Bool = DefaultSettings.listDirs
    public var listFiles: Bool = DefaultSettings.listFiles
    public var listLines: Bool = DefaultSettings.listLines
    public var maxLineLength = DefaultSettings.maxLineLength
    public var multiLineSearch: Bool = DefaultSettings.multiLineSearch
    public var printResults: Bool = DefaultSettings.printResults
    public var printUsage: Bool = DefaultSettings.printUsage
    public var printVersion: Bool = DefaultSettings.printVersion
    public var recursive: Bool = DefaultSettings.recursive
    public var searchArchives: Bool = DefaultSettings.searchArchives
    public var startPath = DefaultSettings.startPath
    public var textFileEncoding = DefaultSettings.textFileEncoding
    public var uniqueLines: Bool = DefaultSettings.uniqueLines
    public var verbose: Bool = DefaultSettings.verbose

    public var inArchiveExtensions = Set<String>()
    public var inArchiveFilePatterns = [Regex]()
    public var inDirPatterns = [Regex]()
    public var inExtensions = Set<String>()
    public var inFilePatterns = [Regex]()
    public var inFileTypes = [FileType]()
    public var inLinesAfterPatterns = [Regex]()
    public var inLinesBeforePatterns = [Regex]()
    public var linesAfterToPatterns = [Regex]()
    public var linesAfterUntilPatterns = [Regex]()
    public var outArchiveExtensions = Set<String>()
    public var outArchiveFilePatterns = [Regex]()
    public var outDirPatterns = [Regex]()
    public var outExtensions = Set<String>()
    public var outFilePatterns = [Regex]()
    public var outFileTypes = [FileType]()
    public var outLinesAfterPatterns = [Regex]()
    public var outLinesBeforePatterns = [Regex]()
    public var searchPatterns = [Regex]()

    public init() {}

    fileprivate func splitExtensions(_ exts: String) -> [String] {
        exts.split { $0 == "," }.map { String($0) }
    }

    public func addInArchiveExtension(_ exts: String) {
        for ext in splitExtensions(exts) {
            inArchiveExtensions.insert(ext)
        }
    }

    public func addInExtension(_ exts: String) {
        for ext in splitExtensions(exts) {
            inExtensions.insert(ext)
        }
    }

    public func addInArchiveFilePattern(_ pattern: String) {
        inArchiveFilePatterns.append(Regex(pattern))
    }

    public func addInDirPattern(_ pattern: String) {
        inDirPatterns.append(Regex(pattern))
    }

    public func addInFilePattern(_ pattern: String) {
        inFilePatterns.append(Regex(pattern))
    }

    public func addInFileType(_ typeName: String) {
        inFileTypes.append(FileTypes.fromName(typeName))
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

    public func addOutArchiveExtension(_ exts: String) {
        for ext in splitExtensions(exts) {
            outArchiveExtensions.insert(ext)
        }
    }

    public func addOutExtension(_ exts: String) {
        for ext in splitExtensions(exts) {
            outExtensions.insert(ext)
        }
    }

    public func addOutArchiveFilePattern(_ pattern: String) {
        outArchiveFilePatterns.append(Regex(pattern))
    }

    public func addOutDirPattern(_ pattern: String) {
        outDirPatterns.append(Regex(pattern))
    }

    public func addOutFilePattern(_ pattern: String) {
        outFilePatterns.append(Regex(pattern))
    }

    public func addOutFileType(_ typeName: String) {
        outFileTypes.append(FileTypes.fromName(typeName))
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

    public var archivesOnly: Bool {
        get {
            _archivesOnly
        }
        set {
            _archivesOnly = newValue
            if newValue {
                searchArchives = newValue
            }
        }
    }

    public var debug: Bool {
        get {
            _debug
        }
        set {
            _debug = newValue
            if newValue {
                verbose = newValue
            }
        }
    }

    public var description: String {
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
