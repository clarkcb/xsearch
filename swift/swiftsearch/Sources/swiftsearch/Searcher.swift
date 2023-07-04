//
//  Searcher.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/20/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation
import swiftfind

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
    let finder: Finder
    let settings: SearchSettings
    private var textFileEncoding: String.Encoding?

    public init(settings: SearchSettings) throws {
        self.settings = settings
        self.finder = try Finder(settings: settings)
        try validateSettings()
    }

    private func strToEncoding(_ encName: String) -> String.Encoding? {
        var encoding: String.Encoding?
        let enc = String.Encoding(rawValue: CFStringConvertEncodingToNSStringEncoding(CFStringConvertIANACharSetNameToEncoding(encName as CFString)))
        if enc.rawValue == 0xFFFF_FFFF {
            encoding = nil
        } else {
            encoding = enc
        }
        return encoding
    }

    private func validateSettings() throws {
        if settings.searchPatterns.isEmpty {
            throw SearchError(msg: "No search patterns defined")
        } else if settings.linesAfter < 0 {
            throw SearchError(msg: "Invalid linesafter")
        } else if settings.linesBefore < 0 {
            throw SearchError(msg: "Invalid linesbefore")
        } else if settings.maxLineLength < 0 {
            throw SearchError(msg: "Invalid maxlinelength")
        } else {
            let textFileEncoding = strToEncoding(settings.textFileEncoding)
            if textFileEncoding == nil {
                throw SearchError(msg: "Invalid textfileencoding")
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

    public func search() throws -> [SearchResult] {
        let fileResults: [FileResult] = finder.find()
        var searchResults = [SearchResult]()

        if settings.verbose {
            let searchDirs = fileResults.map {
                URL(fileURLWithPath: $0.filePath).deletingLastPathComponent().path
            }.sorted().unique()
            logMsg("\nDirectories to be searched (\(searchDirs.count)):")
            for dir in searchDirs {
                logMsg(FileUtil.formatPath(dir, forPaths: Array(settings.paths)))
            }

            logMsg("\nFiles to be searched (\(fileResults.count)):")
            for file in fileResults {
                logMsg(FileUtil.formatPath(file.filePath, forPaths: Array(settings.paths)))
            }
            logMsg("")
        }

        for file in fileResults {
            searchResults += searchFile(file)
        }
        return searchResults
    }

    func searchFile(_ fileResult: FileResult) -> [SearchResult] {
        if fileResult.fileType == FileType.code || fileResult.fileType == FileType.text
            || fileResult.fileType == FileType.xml
        {
            return searchTextFile(fileResult)
        } else if fileResult.fileType == FileType.binary {
            return searchBinaryFile(fileResult)
        } else if fileResult.fileType == FileType.archive {
            return searchArchiveFile(fileResult)
        } else {
            // TODO: handle unknown file type
            return [SearchResult]()
        }
    }

    private func searchTextFile(_ fileResult: FileResult) -> [SearchResult] {
        if settings.multiLineSearch {
            return searchTextFileContents(fileResult)
        } else {
            return searchTextFileLines(fileResult)
        }
    }

    private func searchTextFileContents(_ fileResult: FileResult) -> [SearchResult] {
        var results = [SearchResult]()
        let contents = try? String(contentsOfFile: fileResult.filePath,
                                   encoding: textFileEncoding!)
        if contents != nil {
            let sResults = searchMultiLineString(contents!)
            // add filePath
            for res in sResults {
                let result = SearchResult(
                    searchPattern: res.searchPattern,
                    file: fileResult,
                    lineNum: res.lineNum,
                    matchStartIndex: res.matchStartIndex,
                    matchEndIndex: res.matchEndIndex,
                    line: res.line,
                    linesBefore: res.linesBefore,
                    linesAfter: res.linesAfter
                )
                results.append(result)
            }
        }
        return results
    }

    public func searchMultiLineString(_ str: String) -> [SearchResult] {
        var results = [SearchResult]()
        for pat in settings.searchPatterns {
            results += searchMultiLineStringForPattern(str, regex: pat)
        }
        return results
    }

    private func searchMultiLineStringForPattern(_ str: String, regex: Regex) -> [SearchResult] {
        var results = [SearchResult]()
        let newLineIndices = getNewLineIndices(str)
        let startLineIndices = [0] + newLineIndices.map { $0 + 1 }
        let endLineIndices = newLineIndices + [str.lengthOfBytes(using: String.Encoding.utf8) - 1]
        var matches: [NSTextCheckingResult] = []
        if settings.firstMatch {
            if let match = regex.firstMatch(str) {
                matches = [match]
            }
        } else {
            matches = regex.matches(str)
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
                    searchPattern: regex.pattern,
                    file: nil,
                    lineNum: beforeStartLineIndices.count,
                    matchStartIndex: match.range.location - startLineIndex + 1,
                    matchEndIndex: match.range.location + match.range.length - startLineIndex + 1,
                    line: line,
                    linesBefore: linesBefore,
                    linesAfter: linesAfter
                )
                results.append(result)
            }
        }
        return results
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

    private func searchTextFileLines(_ fileResult: FileResult) -> [SearchResult] {
        var results = [SearchResult]()
        if let reader = StreamReader(path: fileResult.filePath, encoding: textFileEncoding!) {
            let rResults = searchLineReader(reader)
            for res in rResults {
                let result = SearchResult(
                    searchPattern: res.searchPattern,
                    file: fileResult,
                    lineNum: res.lineNum,
                    matchStartIndex: res.matchStartIndex,
                    matchEndIndex: res.matchEndIndex,
                    line: res.line,
                    linesBefore: res.linesBefore,
                    linesAfter: res.linesAfter
                )
                results.append(result)
            }
            reader.close()
        }
        return results
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

    private func searchBinaryFile(_ fileResult: FileResult) -> [SearchResult] {
        var results = [SearchResult]()
        if let data = try? Data(contentsOf: URL(fileURLWithPath: fileResult.filePath)) {
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
                            file: fileResult,
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
                logMsg("Problem encountered creating binary string \(fileResult.description)")
            }
        } else {
            logMsg("Problem encountered reading binary file \(fileResult.description)")
        }
        return results
    }

    private func searchArchiveFile(_ fileResult: FileResult) -> [SearchResult] {
        logMsg("searchArchiveFile(filePath=\"\(fileResult.description)\")")
        return [SearchResult]()
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
    private func getSortedSearchResults(results: [SearchResult]) -> [SearchResult] {
        return results.sorted(by: { self.cmpResultsInDir($0, $1) })
    }
}
