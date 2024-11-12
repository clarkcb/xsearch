//
//  main.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation
import swiftsearch
import swiftfind

func getMatchingDirs(_ results: [SearchResult]) -> [String] {
    results.compactMap(\.file).map {
        URL(fileURLWithPath: $0.filePath).deletingLastPathComponent().path
    }.sorted().unique()
}

func getMatchingFiles(_ results: [SearchResult]) -> [String] {
    results.compactMap(\.file).map(\.filePath).sorted().unique()
}

func getMatchingLines(_ results: [SearchResult], settings: SearchSettings) -> [String] {
    var lines = results.map { $0.line.trimmingCharacters(in: whitespace as CharacterSet) }
    if settings.uniqueLines {
        let lineSet = Set<String>(lines)
        lines = Array(lineSet)
    }
    return lines.sorted { $0.lowercased() < $1.lowercased() }
}

func handleError(_ errorMsg: String, _ options: SearchOptions) {
    logMsg("")
    logError(errorMsg)
    options.usage(1)
}

func handleSearchError(_ error: SearchError, _ options: SearchOptions) {
    handleError(error.msg, options)
}

func handleFindError(_ error: FindError, _ options: SearchOptions) {
    handleError(error.msg, options)
}

func main() {
    let options = SearchOptions()

    let args: [String] = [] + CommandLine.arguments.dropFirst()

    do {
        let settings = try options.settingsFromArgs(args)

        if settings.debug {
            logMsg("\nsettings: \(settings)")
        }

        if settings.printUsage {
            options.usage()
        }

        let searcher = try Searcher(settings: settings)

        let results = try searcher.search()

        if settings.printResults {
            let formatter = SearchResultFormatter(settings: settings)
            logMsg("\nSearch results (\(results.count)):")
            for res in results {
                logMsg("\(formatter.format(result: res))")
            }
        }

        if settings.printDirs {
            let dirs = getMatchingDirs(results)
            logMsg("\nDirectories with matches (\(dirs.count)):")
            for dir in dirs {
                logMsg(FileUtil.formatPath(dir, forPaths: Array(settings.paths)))
            }
        }

        if settings.printFiles {
            let files = getMatchingFiles(results)
            logMsg("\nFiles with matches (\(files.count)):")
            for file in files {
                logMsg(FileUtil.formatPath(file, forPaths: Array(settings.paths)))
            }
        }

        if settings.printLines {
            let lines = getMatchingLines(results, settings: settings)
            let hdr = settings.uniqueLines ? "\nUnique lines with matches (\(lines.count)):"
                : "\nLines with matches (\(lines.count)):"
            logMsg(hdr)
            for line in lines {
                logMsg(line)
            }
        }

    } catch let error as SearchError {
        handleSearchError(error, options)
    } catch let error as FindError {
        handleFindError(error, options)
    } catch {
        logError("Unknown error occurred")
    }
}

main()
