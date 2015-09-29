//
//  main.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

func getMatchingFiles(results: [SearchResult]) -> [String] {
    var files = Set<String>()
    for r in results.filter({$0.file != nil}) {
        let f = r.file!
        files.insert(f)
    }
    return Array(files).sort({$0 < $1})
}

func getMatchingDirs(results: [SearchResult]) -> [String] {
    let files = getMatchingFiles(results)
    var dirs = Set<String>()
    for f in files {
        let dir = f.stringByDeletingLastPathComponent
        dirs.insert(dir)
    }
    return Array(dirs).sort({$0 < $1})
}

func getMatchingLines(results: [SearchResult], settings: SearchSettings) -> [String] {
    var lines = results.map {$0.line.stringByTrimmingCharactersInSet(whitespace)}
    if settings.uniqueLines {
        let lineSet = Set<String>(lines)
        lines = Array(lineSet)
    }
    return lines.sort({$0.lowercaseString < $1.lowercaseString})
}

func main() {
    let options = SearchOptions()

    let args: [String] = [] + Process.arguments.dropFirst()

    var error: NSError?
    let settings = options.settingsFromArgs(args, error: &error)

    if error != nil {
        logMsg("")
        logError(error!.domain)
        options.usage()
    }
    
    if settings.debug {
        logMsg("\nsettings: \(settings)")
    }

    if settings.printUsage {
        options.usage()
    }

    let searcher = Searcher(settings: settings, error: &error)

    if error != nil {
        logMsg("")
        logError(error!.domain)
        options.usage()
    }

    searcher.search(&error)

    if error != nil {
        logMsg("")
        logError(error!.domain)
        options.usage()
    }
    
    let results = searcher.getSearchResults()

    if settings.printResults {
        logMsg("\nSearch results (\(results.count)):")
        for r in results {
            logMsg("\(r)")
        }
    }

    if settings.listDirs {
        let dirs = getMatchingDirs(results)
        logMsg("\nDirectories with matches (\(dirs.count)):")
        for d in dirs {
            logMsg(d)
        }
    }

    if settings.listFiles {
        let files = getMatchingFiles(results)
        logMsg("\nFiles with matches (\(files.count)):")
        for f in files {
            logMsg(f)
        }
    }

    if settings.listLines {
        let lines = getMatchingLines(results, settings: settings)
        let hdr = settings.uniqueLines ? "\nUnique lines with matches (\(lines.count)):"
            : "\nLines with matches (\(lines.count)):"
        logMsg(hdr)
        for l in lines {
            logMsg(l)
        }
    }
}

main()
