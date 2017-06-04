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
fileprivate func < <T : Comparable>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs, rhs) {
  case let (l?, r?):
    return l < r
  case (nil, _?):
    return true
  default:
    return false
  }
}


open class Searcher {
    let fileTypes = FileTypes()
    let settings: SearchSettings
    fileprivate var results = [SearchResult]()

    init(settings: SearchSettings, error: NSErrorPointer) {
        self.settings = settings
        validateSettings(error)
    }

    fileprivate func validateSettings(_ error: NSErrorPointer) {
        if settings.startPath == nil || settings.startPath!.isEmpty {
            setError(error, msg: "Startpath not defined")
        } else if (!FileUtil.exists(settings.startPath!)) {
            setError(error, msg: "Startpath not found")
        } else if settings.searchPatterns.isEmpty {
            setError(error, msg: "No search patterns defined")
        }
    }

    fileprivate func matchesAnyPattern(_ s: String, _ patterns: [Regex]) -> Bool {
        return patterns.filter({$0.test(s)}).count > 0
    }

    fileprivate func anyMatchesAnyPattern(_ ss: [String], _ patterns: [Regex]) -> Bool {
        return ss.filter({self.matchesAnyPattern($0, patterns)}).count > 0
    }

    fileprivate func filterByExtensions(_ ext: String, inExtensions: Set<String>,
        outExtensions: Set<String>) -> Bool {
        return ((inExtensions.isEmpty || inExtensions.contains(ext))
            && (outExtensions.isEmpty || !outExtensions.contains(ext)))
    }

    fileprivate func filterByPatterns(_ s: String, inPatterns: Array<Regex>,
        outPatterns: Array<Regex>) -> Bool {
        return ((inPatterns.isEmpty || matchesAnyPattern(s, Array(inPatterns)))
            && (outPatterns.isEmpty || !matchesAnyPattern(s, Array(outPatterns))))
    }

    func isSearchDir(_ dirPath: String) -> Bool {
        let hidden = FileUtil.splitPath(dirPath).filter {FileUtil.isHidden($0)}
        if hidden.count > 0 && settings.excludeHidden {
            return false
        }
        return filterByPatterns(dirPath, inPatterns: settings.inDirPatterns,
            outPatterns: settings.outDirPatterns)
    }

    func isSearchFile(_ filePath: String) -> Bool {
        if FileUtil.isHiddenFile(URL(fileURLWithPath: filePath).lastPathComponent) && settings.excludeHidden {
            return false
        }
        return (filterByExtensions(FileUtil.getExtension(filePath),
            inExtensions: settings.inExtensions,
            outExtensions: settings.outExtensions)
            && filterByPatterns(filePath, inPatterns: settings.inFilePatterns,
                outPatterns: settings.outFilePatterns))
    }

    func isArchiveSearchFile(_ filePath: String) -> Bool {
        if FileUtil.isHidden(URL(fileURLWithPath: filePath).lastPathComponent) && settings.excludeHidden {
            return false
        }
        return (filterByExtensions(FileUtil.getExtension(filePath),
            inExtensions: settings.inArchiveExtensions,
            outExtensions: settings.outArchiveExtensions)
            && filterByPatterns(filePath, inPatterns: settings.inArchiveFilePatterns,
                outPatterns: settings.outArchiveFilePatterns))
    }

    open func search(_ error: NSErrorPointer) {
        let startPath = settings.startPath!
        if FileUtil.isDirectory(startPath) {
            if isSearchDir(startPath) {
                searchPath(startPath)
            } else {
                setError(error, msg: "Startpath does not match search settings")
            }
        } else if FileUtil.isReadableFile(startPath) {
            if isSearchFile(startPath) {
                searchFile(startPath)
            } else {
                setError(error, msg: "Startpath does not match search settings")
            }
        } else {
            setError(error, msg: "Startpath not readable")
        }
    }

    fileprivate func searchPath(_ filePath: String) {
        var searchDirs = [filePath]
        if settings.recursive {
            searchDirs += getSearchDirs(filePath)
        }
        if settings.verbose {
            logMsg("\nDirectories to be searched (\(searchDirs.count)):")
            for d in searchDirs {
                logMsg(d)
            }
        }

        var searchFiles: [String] = []
        // TODO: get files from directories concurrently
        for d in searchDirs {
            let dirFiles = getSearchFiles(d)
            searchFiles += dirFiles
        }
        if settings.verbose {
            logMsg("\nFiles to be searched (\(searchFiles.count)):")
            for f in searchFiles {
                logMsg(f)
            }
        }
        for f in searchFiles {
            searchFile(f)
        }
    }

    fileprivate func getSearchDirs(_ filePath: String) -> [String] {
        var searchDirs = [String]()
        
        if let enumerator = FileUtil.enumeratorForPath(filePath) {
            while let element = enumerator.nextObject() as? String {
                let fullPath = FileUtil.joinPath(filePath, childPath: element)
                if FileUtil.isDirectory(fullPath) && FileUtil.isReadableFile(fullPath)
                    && isSearchDir(element) {
                    searchDirs.append(fullPath)
                }
            }
        }
        return searchDirs
    }

    // gets search files only directly under the given path
    fileprivate func getSearchFiles(_ filePath: String) -> [String] {
        var searchFiles = [String]()
        let pathFiles = FileUtil.contentsForPath(filePath)
        for f in pathFiles {
            let fullPath = FileUtil.joinPath(filePath, childPath: f)
            if !FileUtil.isDirectory(fullPath) && FileUtil.isReadableFile(fullPath)
                && filterFile(f) {
                    searchFiles.append(fullPath)
            }

        }
        return searchFiles
    }

    fileprivate func filterFile(_ filePath: String) -> Bool {
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

    func searchFile(_ filePath: String) {
        let fileType = fileTypes.getFileType(filePath)
        if fileType == FileType.text {
            searchTextFile(filePath)
        } else if fileType == FileType.binary {
            searchBinaryFile(filePath)
        } else if fileType == FileType.archive {
            searchArchiveFile(filePath)
        }
    }

    fileprivate func searchTextFile(_ filePath: String) {
        if settings.multiLineSearch {
            searchTextFileContents(filePath)
        } else {
            searchTextFileLines(filePath)
        }
    }

    fileprivate func searchTextFileContents(_ filePath: String) {
        let contents = try? String(contentsOfFile: filePath,
            encoding: String.Encoding.utf8)
        if contents != nil {
            let results = searchMultiLineString(contents!)
            // add filePath
            for r in results {
                let result = SearchResult(
                    searchPattern: r.searchPattern,
                    file: filePath,
                    lineNum: r.lineNum,
                    matchStartIndex: r.matchStartIndex,
                    matchEndIndex: r.matchEndIndex,
                    line: r.line,
                    linesBefore: r.linesBefore,
                    linesAfter: r.linesAfter)
                addSearchResult(result)
            }
        }
    }

    open func searchMultiLineString(_ s: String) -> [SearchResult] {
        var sResults = [SearchResult]()
        for p in settings.searchPatterns {
            sResults += searchMultiLineStringForPattern(s, pattern: p)
        }
        return sResults
    }

    fileprivate func searchMultiLineStringForPattern(_ s: String, pattern: Regex) -> [SearchResult] {
        var spResults = [SearchResult]()
        let newLineIndices = getNewLineIndices(s)
        let startLineIndices = [0] + newLineIndices.map {$0+1}
        let endLineIndices = newLineIndices + [s.characters.count - 1]
        var matches = pattern.matches(s)
        if matches.count > 0 && settings.firstMatch {
            matches = [matches[0]]
        }
        for m in matches {
            let beforeStartLineIndices = startLineIndices.filter({$0 < m.range.location})
            let startLineIndex = beforeStartLineIndices[beforeStartLineIndices.count - 1]
            let endLineIndex = endLineIndices[beforeStartLineIndices.count - 1]
            let line = lineFromIndices(s, startLineIndex: startLineIndex, endLineIndex: endLineIndex)
            var linesBefore: [String] = []
            if settings.linesBefore > 0 {
                var linesBeforeStartIndices = takeRight(beforeStartLineIndices
                    .filter({$0 < startLineIndex}), num: settings.linesBefore)
                var linesBeforeEndIndices = takeRight(endLineIndices.filter({$0 < endLineIndex}),
                    num: settings.linesBefore)
                for i in 0 ..< linesBeforeStartIndices.count {
                    linesBefore.append(lineFromIndices(s, startLineIndex: linesBeforeStartIndices[i],
                        endLineIndex: linesBeforeEndIndices[i]))
                }
            }
            var linesAfter: [String] = []
            if settings.linesAfter > 0 {
                let linesAfterStartIndices = take(startLineIndices.filter({$0 > startLineIndex}),
                    num: settings.linesAfter)
                let linesAfterEndIndices = take(endLineIndices.filter({$0 > endLineIndex}),
                    num: settings.linesAfter)
                for i in 0 ..< linesAfterStartIndices.count {
                    linesAfter.append(lineFromIndices(s, startLineIndex: linesAfterStartIndices[i],
                        endLineIndex: linesAfterEndIndices[i]))
                }
            }

            if (linesBefore.isEmpty || linesBeforeMatch(linesBefore))
                && (linesAfter.isEmpty || linesAfterMatch(linesAfter)) {
                let result = SearchResult(
                    searchPattern: pattern.pattern,
                    file: "",
                    lineNum: beforeStartLineIndices.count,
                    matchStartIndex: m.range.location - startLineIndex + 1,
                    matchEndIndex: m.range.location + m.range.length - startLineIndex + 1,
                    line: line,
                    linesBefore: linesBefore,
                    linesAfter: linesAfter)
                spResults.append(result)
            }
        }
        return spResults
    }

    fileprivate func linesMatch(_ lines: [String], _ inPatterns: [Regex], _ outPatterns: [Regex]) -> Bool {
        return (inPatterns.isEmpty || anyMatchesAnyPattern(lines, inPatterns))
            && (outPatterns.isEmpty || !anyMatchesAnyPattern(lines, outPatterns))
    }

    fileprivate func linesBeforeMatch(_ linesBefore: [String]) -> Bool {
        return linesBefore.isEmpty || linesMatch(linesBefore, settings.inLinesBeforePatterns,
            settings.outLinesBeforePatterns)
    }

    fileprivate func linesAfterMatch(_ linesAfter: [String]) -> Bool {
        return linesAfter.isEmpty || linesMatch(linesAfter, settings.inLinesAfterPatterns,
            settings.outLinesAfterPatterns)
    }

    fileprivate func lineFromIndices(_ s: String, startLineIndex: Int, endLineIndex: Int) -> String {
        let startLineStringIndex = s.characters.index(s.startIndex, offsetBy: startLineIndex)
        let endLineStringIndex = s.characters.index(s.startIndex, offsetBy: endLineIndex)
        let lineRange = Range<String.Index>(startLineStringIndex ..< endLineStringIndex)
        return s.substring(with: lineRange)
    }

    fileprivate func getNewLineIndices(_ s: String) -> [Int] {
        var indices = [Int]()
        var currentIndex = 0
        for c in s.characters {
            if c == "\n" {
                indices.append(currentIndex)
            }
            currentIndex += 1
        }
        return indices
    }

    fileprivate func searchTextFileLines(_ filePath: String) {
        var results: [SearchResult] = []
        if let reader = StreamReader(path: filePath) {
            results = searchLineReader(reader)
            for r in results {
                let result = SearchResult(
                    searchPattern: r.searchPattern,
                    file: filePath,
                    lineNum: r.lineNum,
                    matchStartIndex: r.matchStartIndex,
                    matchEndIndex: r.matchEndIndex,
                    line: r.line,
                    linesBefore: r.linesBefore,
                    linesAfter: r.linesAfter)
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
            } else if settings.firstMatch && settings.searchPatterns.count == matchedPatterns.count {
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
                let searchPatterns = settings.searchPatterns.filter({!matchedPatterns.contains($0.pattern)})
                for p in searchPatterns {
                    let matches = p.matches(line!)
                    for m in matches {
                        if (linesBefore.isEmpty || linesBeforeMatch(linesBefore))
                        && (linesAfter.isEmpty || linesAfterMatch(linesAfter)) {
                            let result = SearchResult(
                                searchPattern: p.pattern,
                                file: "",
                                lineNum: lineNum,
                                matchStartIndex: m.range.location + 1,
                                matchEndIndex: m.range.location + m.range.length + 1,
                                line: line!,
                                linesBefore: linesBefore,
                                linesAfter: linesAfter)
                            results.append(result)
                            if settings.firstMatch {
                                matchedPatterns.insert(p.pattern)
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

    fileprivate func searchBinaryFile(_ filePath: String) {
        if let data = try? Data(contentsOf: URL(fileURLWithPath: filePath)) {
            // convert to a string using (any) single-byte encoding, using UTF8
            // should cause conversion problems
            if let ds = NSString(data: data, encoding: String.Encoding.isoLatin1.rawValue) {
                for p in settings.searchPatterns {
                    var matches = p.matches(ds as String)
                    if matches.count > 0 && settings.firstMatch {
                        matches = [matches[0]]
                    }
                    for m in matches {
                        let result = SearchResult(
                            searchPattern: p.pattern,
                            file: filePath,
                            lineNum: 0,
                            matchStartIndex: m.range.location + 1,
                            matchEndIndex: m.range.location + m.range.length + 1,
                            line: "",
                            linesBefore: [],
                            linesAfter: [])
                        results.append(result)
                    }
                }
            } else {
                logMsg("Problem encountered creating binary string \(filePath)")
            }
        } else {
            logMsg("Problem encountered reading binary file \(filePath)")
        }
    }

    fileprivate func searchArchiveFile(_ filePath: String) {
        logMsg("searchArchiveFile(filePath=\"\(filePath)\")")
    }

    fileprivate func addSearchResult(_ result: SearchResult) {
        results.append(result)
    }
    
    fileprivate func cmpResultsInDir(_ r1: SearchResult, _ r2: SearchResult) -> Bool {
        let path1 = NSURL(fileURLWithPath: r1.file!).deletingLastPathComponent?.absoluteString
        let path2 = NSURL(fileURLWithPath: r2.file!).deletingLastPathComponent?.absoluteString
        if path1 == path2 {
            let file1 = NSURL(fileURLWithPath: r1.file!).lastPathComponent!.lowercased()
            let file2 = NSURL(fileURLWithPath: r2.file!).lastPathComponent!.lowercased()
            if file1 == file2 {
                if r1.lineNum == r2.lineNum {
                    return r1.matchStartIndex < r2.matchStartIndex
                }
                return r1.lineNum < r2.lineNum
            }
            return file1 < file2
        }
        return path1 < path2
    }

    // results in swift are already sorted the way I want (case-insensitive by path, then filename,
    // then lineNum, then matchStartIndex). This method is an example to reference for languages
    // that require sorting (go, etc.)
    fileprivate func getSortedSearchResults() -> [SearchResult] {
        return results.sorted(by: {self.cmpResultsInDir($0, $1)})
    }
    
    open func getSearchResults() -> [SearchResult] {
        return results
    }
}
