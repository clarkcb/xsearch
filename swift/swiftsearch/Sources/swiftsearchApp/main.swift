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
            if results.count > 0 {
                let formatter = SearchResultFormatter(settings: settings)
                logMsg("\nSearch results (\(results.count)):")
                for res in results {
                    logMsg("\(formatter.format(result: res))")
                }
            } else {
                logMsg("\nSearch results: 0")
            }
        }

        if settings.printDirs {
            let dirs = getMatchingDirs(results)
            if dirs.count > 0 {
                logMsg("\nMatching directories (\(dirs.count)):")
                for dir in dirs {
                    // TODO: better way to format paths, probably create a FindPath class for this
                    // logMsg(FileUtil.formatPath(dir, forPaths: Array(settings.paths)))
                    logMsg(dir)
                }
            } else {
                logMsg("\nMatching directories: 0")
            }
        }

        if settings.printFiles {
            let files = getMatchingFiles(results)
            if files.count > 0 {
                logMsg("\nMatching files (\(files.count)):")
                for file in files {
                    // TODO: better way to format paths, probably create a FindPath class for this
                    // logMsg(FileUtil.formatPath(file, forPaths: Array(settings.paths)))
                    logMsg(file)
                }
            } else {
                logMsg("\nMatching files: 0")
            }
        }

        if settings.printLines {
            let lines = getMatchingLines(results, settings: settings)
            let hdr = settings.uniqueLines ? "\nUnique matching lines"
                : "\nMatching lines"
            if lines.count > 0 {
                logMsg("\(hdr) (\(lines.count)):")
                for line in lines {
                    logMsg(line)
                }
            } else {
                logMsg("\(hdr): 0")
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
