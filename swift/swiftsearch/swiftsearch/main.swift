//
//  main.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

func getMatchingFiles(_ results: [SearchResult]) -> [String] {
    var files = Set<String>()
    for res in results.filter({ $0.file != nil }) {
        files.insert(res.file!.description())
    }
    return Array(files).sorted { $0 < $1 }
}

func getMatchingDirs(_ results: [SearchResult]) -> [String] {
    let files = getMatchingFiles(results)
    var dirs = Set<String>()
    for file in files {
        let dir = NSURL(fileURLWithPath: file).deletingLastPathComponent?.absoluteString
        dirs.insert(dir!)
    }
    return Array(dirs).sorted { $0 < $1 }
}

func getMatchingLines(_ results: [SearchResult], settings: SearchSettings) -> [String] {
    var lines = results.map { $0.line.trimmingCharacters(in: whitespace as CharacterSet) }
    if settings.uniqueLines {
        let lineSet = Set<String>(lines)
        lines = Array(lineSet)
    }
    return lines.sorted { $0.lowercased() < $1.lowercased() }
}

func handleError(_ error: NSError, _ options: SearchOptions) {
    logMsg("")
    logError(error.domain)
    options.usage(1)
}

func main() {
    let options = SearchOptions()

    let args: [String] = [] + CommandLine.arguments.dropFirst()

    var error: NSError?
    let settings = options.settingsFromArgs(args, error: &error)

    if error != nil {
        handleError(error!, options)
    }

    if settings.debug {
        logMsg("\nsettings: \(settings)")
    }

    if settings.printUsage {
        options.usage()
    }

    let searcher = Searcher(settings: settings, error: &error)

    if error != nil {
        handleError(error!, options)
    }

    searcher.search(&error)

    if error != nil {
        handleError(error!, options)
    }

    let results = searcher.getSearchResults()

    if settings.printResults {
        let formatter = SearchResultFormatter(settings: settings)
        logMsg("\nSearch results (\(results.count)):")
        for res in results {
            logMsg("\(formatter.format(result: res))")
        }
    }

    if settings.listDirs {
        let dirs = getMatchingDirs(results)
        logMsg("\nDirectories with matches (\(dirs.count)):")
        for dir in dirs {
            logMsg(dir)
        }
    }

    if settings.listFiles {
        let files = getMatchingFiles(results)
        logMsg("\nFiles with matches (\(files.count)):")
        for file in files {
            logMsg(file)
        }
    }

    if settings.listLines {
        let lines = getMatchingLines(results, settings: settings)
        let hdr = settings.uniqueLines ? "\nUnique lines with matches (\(lines.count)):"
            : "\nLines with matches (\(lines.count)):"
        logMsg(hdr)
        for line in lines {
            logMsg(line)
        }
    }
}

main()
