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

func handleError(_ errorMsg: String, _ colorize: Bool, _ options: SearchOptions) {
    logMsg("")
    logError(errorMsg, colorize: colorize)
    options.usage(1)
}

func handleSearchError(_ error: SearchError, _ colorize: Bool, _ options: SearchOptions) {
    handleError(error.msg, colorize, options)
}

func handleFindError(_ error: FindError, _ colorize: Bool, _ options: SearchOptions) {
    handleError(error.msg, colorize, options)
}

func main() {
    var colorize = true
    let options = SearchOptions()

    let args: [String] = [] + CommandLine.arguments.dropFirst()

    do {
        let settings = try options.settingsFromArgs(args)
        colorize = settings.colorize

        if settings.debug {
            logMsg("\nsettings: \(settings)")
        }

        if settings.printUsage {
            options.usage()
        }

        let searcher = try Searcher(settings: settings)

        let results = try searcher.search()
        let formatter = SearchResultFormatter(settings: settings)

        if settings.printResults {
            searcher.printSearchResults(results, formatter)
        }

        if settings.printDirs {
            searcher.printMatchingDirs(results, formatter)
        }

        if settings.printFiles {
            searcher.printMatchingFiles(results, formatter)
        }

        if settings.printLines {
            searcher.printMatchingLines(results, formatter)
        }

        if settings.printMatches {
            searcher.printMatches(results, formatter)
        }

    } catch let error as SearchError {
        handleSearchError(error, colorize, options)
    } catch let error as FindError {
        handleFindError(error, colorize, options)
    } catch {
        logError("Unknown error occurred")
    }
}

main()
