//
//  Searcher.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/20/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation
// FIXME: comparison operators with optionals were removed from the Swift Standard Libary.
// Consider refactoring the code to use the non-optional operators.
private func < <T: Comparable>(lhs: T?, rhs: T?) -> Bool {
    switch (lhs, rhs) {
    case let (l?, r?):
        return l < r
    case (nil, _?):
        return true
    default:
        return false
    }
}

public class Searcher {
    let fileTypes = FileTypes()
    let settings: SearchSettings
    private var results = [SearchResult]()
    private var textFileEncoding: String.Encoding?

    public init(settings: SearchSettings, error: NSErrorPointer) {
        self.settings = settings
        validateSettings(error)
    }

    private func strToEncoding(_ encName: String) -> String.Encoding? {
        // working (non-exhaustively) from available encodings: https://developer.apple.com/documentation/swift/string/encoding
        var encoding: String.Encoding?
        switch encName.lowercased() {
        case "utf-8", "utf8":
            encoding = String.Encoding.utf8
        case "utf-16", "utf16":
            encoding = String.Encoding.utf16
        case "utf-32", "utf32":
            encoding = String.Encoding.utf32
        case "iso-8859-1", "iso88591", "iso-latin-1", "isolatin1":
            encoding = String.Encoding.isoLatin1
        case "macosroman":
            encoding = String.Encoding.macOSRoman
        case "windows-1252", "windows1252", "cp-1252", "cp1252":
            encoding = String.Encoding.windowsCP1252
        case "shift-jis", "shiftjis":
            encoding = String.Encoding.shiftJIS
        case "ascii":
            encoding = String.Encoding.ascii
        default:
            encoding = nil
        }
        return encoding
    }

    private func validateSettings(_ error: NSErrorPointer) {
        if settings.startPath == nil || settings.startPath!.isEmpty {
            setError(error, msg: "Startpath not defined")
        } else if !FileUtil.exists(settings.startPath!) {
            setError(error, msg: "Startpath not found")
        } else if !FileUtil.isReadableFile(settings.startPath!) {
            setError(error, msg: "Startpath not readable")
        } else if settings.searchPatterns.isEmpty {
            setError(error, msg: "No search patterns defined")
        } else if settings.linesAfter < 0 {
            setError(error, msg: "Invalid linesafter")
        } else if settings.linesBefore < 0 {
            setError(error, msg: "Invalid linesbefore")
        } else if settings.maxLineLength < 0 {
            setError(error, msg: "Invalid maxlinelength")
        } else {
            let textFileEncoding = strToEncoding(settings.textFileEncoding)
            if textFileEncoding == nil {
                setError(error, msg: "Invalid textfileencoding")
            } else {
                self.textFileEncoding = textFileEncoding
            }
        }
    }

    private func matchesAnyPattern(_ str: String, _ patterns: [Regex]) -> Bool {
        patterns.filter { $0.test(str) }.count > 0
    }

    private func anyMatchesAnyPattern(_ strs: [String], _ patterns: [Regex]) -> Bool {
        strs.filter { self.matchesAnyPattern($0, patterns) }.count > 0
    }

    private func filterByExtensions(_ ext: String, inExtensions: Set<String>,
                                    outExtensions: Set<String>) -> Bool
    {
        ((inExtensions.isEmpty || inExtensions.contains(ext))
            && (outExtensions.isEmpty || !outExtensions.contains(ext)))
    }

    private func filterByPatterns(_ str: String, inPatterns: [Regex],
                                  outPatterns: [Regex]) -> Bool
    {
        ((inPatterns.isEmpty || matchesAnyPattern(str, Array(inPatterns)))
            && (outPatterns.isEmpty || !matchesAnyPattern(str, Array(outPatterns))))
    }

    private func filterByFileTypes(_ fileType: FileType, inFileTypes: [FileType],
                                   outFileTypes: [FileType]) -> Bool
    {
        ((inFileTypes.isEmpty || inFileTypes.contains(fileType))
            && (outFileTypes.isEmpty || !outFileTypes.contains(fileType)))
    }

    public func isSearchDir(_ dirPath: String) -> Bool {
        if FileUtil.isHidden(dirPath), settings.excludeHidden {
            return false
        }
        return filterByPatterns(dirPath, inPatterns: settings.inDirPatterns,
                                outPatterns: settings.outDirPatterns)
    }

    public func isSearchFile(_ filePath: String) -> Bool {
        if FileUtil.isHiddenFile(URL(fileURLWithPath: filePath).lastPathComponent), settings.excludeHidden {
            return false
        }
        return (filterByExtensions(FileUtil.getExtension(filePath),
                                   inExtensions: settings.inExtensions,
                                   outExtensions: settings.outExtensions)
                && filterByPatterns(filePath,
                                    inPatterns: settings.inFilePatterns,
                                    outPatterns: settings.outFilePatterns)
                && filterByFileTypes(fileTypes.getFileType(filePath),
                                     inFileTypes: settings.inFileTypes,
                                     outFileTypes: settings.outFileTypes))
    }

    public func isArchiveSearchFile(_ filePath: String) -> Bool {
        if FileUtil.isHidden(URL(fileURLWithPath: filePath).lastPathComponent), settings.excludeHidden {
            return false
        }
        return (filterByExtensions(FileUtil.getExtension(filePath),
                                   inExtensions: settings.inArchiveExtensions,
                                   outExtensions: settings.outArchiveExtensions)
                && filterByPatterns(filePath, inPatterns: settings.inArchiveFilePatterns,
                                    outPatterns: settings.outArchiveFilePatterns))
    }

    public func search(_ error: NSErrorPointer) {
        let startPath = settings.startPath!
        if FileUtil.isDirectory(startPath) {
            if isSearchDir(startPath) {
                searchPath(startPath)
            } else {
                setError(error, msg: "Startpath does not match search settings")
            }
        } else if FileUtil.isReadableFile(startPath) {
            if isSearchFile(startPath) {
                searchFile(SearchFile(filePath: startPath, fileType: fileTypes.getFileType(startPath)))
            } else {
                setError(error, msg: "Startpath does not match search settings")
            }
        } else {
            setError(error, msg: "Startpath not readable")
        }
    }

    private func searchPath(_ filePath: String) {
        var searchFiles: [SearchFile] = getSearchFiles(filePath)
        searchFiles = searchFiles.sorted(by: { $0.filePath < $1.filePath })

        if settings.verbose {
            let searchDirs = searchFiles.map {
                URL(fileURLWithPath: $0.filePath).deletingLastPathComponent().path
            }.sorted().unique()
            logMsg("\nDirectories to be searched (\(searchDirs.count)):")
            for dir in searchDirs {
                logMsg(dir)
            }

            logMsg("\nFiles to be searched (\(searchFiles.count)):")
            for file in searchFiles {
                logMsg(file.description)
            }
            logMsg("")
        }

        for file in searchFiles {
            searchFile(file)
        }
    }

    // gets all SearchFiles recursively
    private func getSearchFiles(_ filePath: String) -> [SearchFile] {
        var searchFiles = [SearchFile]()
        if let enumerator = FileUtil.enumerator(forPath: filePath, settings: settings) {
            for case let fileURL as URL in enumerator {
                do {
                    let fileAttributes = try fileURL.resourceValues(forKeys: [.isDirectoryKey, .isRegularFileKey])
                    if fileAttributes.isDirectory! {
                        if !isSearchDir(fileURL.path) {
                            enumerator.skipDescendents()
                        }
                    } else if fileAttributes.isRegularFile! {
                        let searchFile = filterToSearchFile(fileURL.path)
                        if searchFile != nil {
                            searchFiles.append(searchFile!)
                        }
                    }
                } catch { print(error, fileURL) }
            }
        }
        return searchFiles
    }

    public func filterToSearchFile(_ filePath: String) -> SearchFile? {
        let fileType = fileTypes.getFileType(filePath)
        if fileType == FileType.unknown {
            return nil
        }
        if (fileType == FileType.archive && settings.searchArchives && isArchiveSearchFile(filePath))
            || (!settings.archivesOnly && isSearchFile(filePath)) {
            return SearchFile(filePath: filePath, fileType: fileType)
        }
        return nil
    }

    public func filterFile(_ filePath: String) -> Bool {
        let fileType = fileTypes.getFileType(filePath)
        if fileType == FileType.unknown {
            return false
        }
        if fileType == FileType.archive {
            return settings.searchArchives && isArchiveSearchFile(filePath)
        }
        // fileType == FileType.Text || fileType == FileType.Binary
        return !settings.archivesOnly && isSearchFile(filePath)
    }

    func searchFile(_ searchFile: SearchFile) {
        if settings.debug {
            logMsg("Searching file: \(searchFile.description)")
        }
        if searchFile.fileType == FileType.code || searchFile.fileType == FileType.text
            || searchFile.fileType == FileType.xml {
            searchTextFile(searchFile)
        } else if searchFile.fileType == FileType.binary {
            searchBinaryFile(searchFile)
        } else if searchFile.fileType == FileType.archive {
            searchArchiveFile(searchFile)
        }
    }

    private func searchTextFile(_ searchFile: SearchFile) {
        if settings.multiLineSearch {
            searchTextFileContents(searchFile)
        } else {
            searchTextFileLines(searchFile)
        }
    }

    private func searchTextFileContents(_ searchFile: SearchFile) {
        let contents = try? String(contentsOfFile: searchFile.filePath,
                                   encoding: self.textFileEncoding!)
        if contents != nil {
            let results = searchMultiLineString(contents!)
            // add filePath
            for res in results {
                let result = SearchResult(
                    searchPattern: res.searchPattern,
                    file: searchFile,
                    lineNum: res.lineNum,
                    matchStartIndex: res.matchStartIndex,
                    matchEndIndex: res.matchEndIndex,
                    line: res.line,
                    linesBefore: res.linesBefore,
                    linesAfter: res.linesAfter
                )
                addSearchResult(result)
            }
        }
    }

    public func searchMultiLineString(_ str: String) -> [SearchResult] {
        var sResults = [SearchResult]()
        for pat in settings.searchPatterns {
            sResults += searchMultiLineStringForPattern(str, pattern: pat)
        }
        return sResults
    }

    private func searchMultiLineStringForPattern(_ str: String, pattern: Regex) -> [SearchResult] {
        var spResults = [SearchResult]()
        let newLineIndices = getNewLineIndices(str)
        let startLineIndices = [0] + newLineIndices.map { $0 + 1 }
        let endLineIndices = newLineIndices + [str.lengthOfBytes(using: String.Encoding.utf8) - 1]
        var matches = pattern.matches(str)
        if matches.count > 0, settings.firstMatch {
            matches = [matches[0]]
        }
        for match in matches {
            let beforeStartLineIndices = startLineIndices.filter { $0 < match.range.location }
            let startLineIndex = beforeStartLineIndices[beforeStartLineIndices.count - 1]
            let endLineIndex = endLineIndices[beforeStartLineIndices.count - 1]
            let line = lineFromIndices(str, startLineIndex: startLineIndex, endLineIndex: endLineIndex)
            var linesBefore: [String] = []
            if settings.linesBefore > 0 {
                let linesBeforeStartIndices = takeRight(beforeStartLineIndices
                    .filter { $0 < startLineIndex }, num: settings.linesBefore)
                let linesBeforeEndIndices = takeRight(endLineIndices.filter { $0 < endLineIndex },
                                                      num: settings.linesBefore)
                for i in 0 ..< linesBeforeStartIndices.count {
                    linesBefore.append(lineFromIndices(str, startLineIndex: linesBeforeStartIndices[i],
                                                       endLineIndex: linesBeforeEndIndices[i]))
                }
            }
            var linesAfter: [String] = []
            if settings.linesAfter > 0 {
                let linesAfterStartIndices = take(startLineIndices.filter { $0 > startLineIndex },
                                                  num: settings.linesAfter)
                let linesAfterEndIndices = take(endLineIndices.filter { $0 > endLineIndex },
                                                num: settings.linesAfter)
                for i in 0 ..< linesAfterStartIndices.count {
                    linesAfter.append(lineFromIndices(str, startLineIndex: linesAfterStartIndices[i],
                                                      endLineIndex: linesAfterEndIndices[i]))
                }
            }

            if linesBefore.isEmpty || linesBeforeMatch(linesBefore),
               linesAfter.isEmpty || linesAfterMatch(linesAfter)
            {
                let result = SearchResult(
                    searchPattern: pattern.pattern,
                    file: nil,
                    lineNum: beforeStartLineIndices.count,
                    matchStartIndex: match.range.location - startLineIndex + 1,
                    matchEndIndex: match.range.location + match.range.length - startLineIndex + 1,
                    line: line,
                    linesBefore: linesBefore,
                    linesAfter: linesAfter
                )
                spResults.append(result)
            }
        }
        return spResults
    }

    private func linesMatch(_ lines: [String], _ inPatterns: [Regex], _ outPatterns: [Regex]) -> Bool {
        (inPatterns.isEmpty || anyMatchesAnyPattern(lines, inPatterns))
            && (outPatterns.isEmpty || !anyMatchesAnyPattern(lines, outPatterns))
    }

    private func linesBeforeMatch(_ linesBefore: [String]) -> Bool {
        linesBefore.isEmpty || linesMatch(linesBefore, settings.inLinesBeforePatterns,
                                          settings.outLinesBeforePatterns)
    }

    private func linesAfterMatch(_ linesAfter: [String]) -> Bool {
        linesAfter.isEmpty || linesMatch(linesAfter, settings.inLinesAfterPatterns,
                                         settings.outLinesAfterPatterns)
    }

    private func lineFromIndices(_ str: String, startLineIndex: Int, endLineIndex: Int) -> String {
        let startLineStringIndex = str.index(str.startIndex, offsetBy: startLineIndex)
        let endLineStringIndex = str.index(str.startIndex, offsetBy: endLineIndex)
        return String(str[startLineStringIndex ..< endLineStringIndex])
    }

    private func getNewLineIndices(_ str: String) -> [Int] {
        var indices = [Int]()
        var currentIndex = 0
        for c in str {
            if c == "\n" {
                indices.append(currentIndex)
            }
            currentIndex += 1
        }
        return indices
    }

    private func searchTextFileLines(_ searchFile: SearchFile) {
        let results: [SearchResult]
        if let reader = StreamReader(path: searchFile.filePath, encoding: self.textFileEncoding!) {
            results = searchLineReader(reader)
            for res in results {
                let result = SearchResult(
                    searchPattern: res.searchPattern,
                    file: searchFile,
                    lineNum: res.lineNum,
                    matchStartIndex: res.matchStartIndex,
                    matchEndIndex: res.matchEndIndex,
                    line: res.line,
                    linesBefore: res.linesBefore,
                    linesAfter: res.linesAfter
                )
                addSearchResult(result)
            }
            reader.close()
        }
    }

    open func searchLineReader(_ reader: StreamReader) -> [SearchResult] {
        var stop = false
        var lineNum = 0
        var matchedPatterns: Set<String> = []
        var results: [SearchResult] = []
        var linesBefore: [String] = []
        var linesAfter: [String] = []
        while !stop {
            lineNum += 1
            var line: String?
            if linesAfter.count > 0 {
                line = linesAfter.remove(at: 0) as String?
            } else {
                line = reader.nextLine()
            }
            if line == nil {
                stop = true
            } else if settings.firstMatch, settings.searchPatterns.count == matchedPatterns.count {
                stop = true
            } else {
                if settings.linesAfter > 0 {
                    while linesAfter.count < settings.linesAfter {
                        if let lineAfter = reader.nextLine() {
                            linesAfter.append(lineAfter)
                        } else {
                            break
                        }
                    }
                }
                let searchPatterns = settings.searchPatterns.filter { !matchedPatterns.contains($0.pattern) }
                for pat in searchPatterns {
                    let matches = pat.matches(line!)
                    for match in matches {
                        if linesBefore.isEmpty || linesBeforeMatch(linesBefore),
                           linesAfter.isEmpty || linesAfterMatch(linesAfter)
                        {
                            let result = SearchResult(
                                searchPattern: pat.pattern,
                                file: nil,
                                lineNum: lineNum,
                                matchStartIndex: match.range.location + 1,
                                matchEndIndex: match.range.location + match.range.length + 1,
                                line: line!,
                                linesBefore: linesBefore,
                                linesAfter: linesAfter
                            )
                            results.append(result)
                            if settings.firstMatch {
                                matchedPatterns.insert(pat.pattern)
                            }
                        }
                    }
                }
                if settings.linesBefore > 0 {
                    if linesBefore.count == settings.linesBefore {
                        linesBefore.remove(at: 0)
                    }
                    if linesBefore.count < settings.linesBefore {
                        linesBefore.append(line!)
                    }
                }
            }
        }
        return results
    }

    private func searchBinaryFile(_ searchFile: SearchFile) {
        if let data = try? Data(contentsOf: URL(fileURLWithPath: searchFile.filePath)) {
            // convert to a string using (any) single-byte encoding, using UTF8
            // should cause conversion problems
            if let dstr = NSString(data: data, encoding: String.Encoding.isoLatin1.rawValue) {
                for pat in settings.searchPatterns {
                    var matches = pat.matches(dstr as String)
                    if matches.count > 0, settings.firstMatch {
                        matches = [matches[0]]
                    }
                    for match in matches {
                        let result = SearchResult(
                            searchPattern: pat.pattern,
                            file: searchFile,
                            lineNum: 0,
                            matchStartIndex: match.range.location + 1,
                            matchEndIndex: match.range.location + match.range.length + 1,
                            line: "",
                            linesBefore: [],
                            linesAfter: []
                        )
                        results.append(result)
                    }
                }
            } else {
                logMsg("Problem encountered creating binary string \(searchFile.description)")
            }
        } else {
            logMsg("Problem encountered reading binary file \(searchFile.description)")
        }
    }

    private func searchArchiveFile(_ searchFile: SearchFile) {
        logMsg("searchArchiveFile(filePath=\"\(searchFile.description)\")")
    }

    private func addSearchResult(_ result: SearchResult) {
        results.append(result)
    }

    private func cmpResultsInDir(_ res1: SearchResult, _ res2: SearchResult) -> Bool {
        let path1 = NSURL(fileURLWithPath: res1.file!.filePath).deletingLastPathComponent?.absoluteString
        let path2 = NSURL(fileURLWithPath: res2.file!.filePath).deletingLastPathComponent?.absoluteString
        if path1 == path2 {
            let file1 = NSURL(fileURLWithPath: res1.file!.filePath).lastPathComponent!.lowercased()
            let file2 = NSURL(fileURLWithPath: res2.file!.filePath).lastPathComponent!.lowercased()
            if file1 == file2 {
                if res1.lineNum == res2.lineNum {
                    return res1.matchStartIndex < res2.matchStartIndex
                }
                return res1.lineNum < res2.lineNum
            }
            return file1 < file2
        }
        return path1 < path2
    }

    // if results weren't in DESC order, would use this method to sort them
    private func getSortedSearchResults() -> [SearchResult] {
        results.sorted(by: { self.cmpResultsInDir($0, $1) })
    }

    public func getSearchResults() -> [SearchResult] {
//        return results.reversed() // reverse to get ASC order
        results // reverse to get ASC order
    }
}
