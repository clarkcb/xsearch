//
//  SearchOptions.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/17/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation
import swiftfind

struct SearchOption: Option {
    let shortArg: String?
    let longArg: String
    let desc: String
    let argType: ArgTokenType

    var sortArg: String {
        if shortArg != nil, !shortArg!.isEmpty {
            return shortArg!.lowercased() + "@" + longArg.lowercased()
        }
        return longArg.lowercased()
    }
}

public class SearchOptions {
    private var config: SearchConfig
    private var searchOptions = [SearchOption]()
    private var argTokenizer: ArgTokenizer?

    public init() {
        self.config = SearchConfig()
        setSearchOptionsFromJson()
        argTokenizer = ArgTokenizer(searchOptions)
    }

    private func setSearchOptionsFromJson() {
        do {
            let searchOptionsUrl = URL(fileURLWithPath: config.searchOptionsPath)
            let data = try Data(contentsOf: searchOptionsUrl, options: .mappedIfSafe)
            if let json = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any] {
                if let options = json["searchoptions"] as? [[String: Any]] {
                    for so in options {
                        let longArg = so["long"] as! String
                        let shortArg = so.index(forKey: "short") != nil ? so["short"] as! String : ""
                        let desc = so["desc"] as! String
                        var argType = ArgTokenType.unknown
                        if self.boolActionDict.index(forKey: longArg) != nil {
                            argType = ArgTokenType.bool
                        } else if self.stringActionDict.index(forKey: longArg) != nil {
                            argType = ArgTokenType.str
                        } else if self.intActionDict.index(forKey: longArg) != nil {
                            argType = ArgTokenType.int
                        } else if self.longActionDict.index(forKey: longArg) != nil {
                            argType = ArgTokenType.long
                        }
                        searchOptions.append(SearchOption(shortArg: shortArg, longArg: longArg, desc: desc, argType: argType))
                    }
                }
            }
        } catch let error as NSError {
            print("Failed to load: \(error.localizedDescription)")
        }
    }

    private let boolActionDict: [String: (Bool, SearchSettings) -> Void] = [
        "allmatches": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.firstMatch = !bool
        },
        "archivesonly": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.archivesOnly = bool
        },
        "colorize": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.colorize = bool
        },
        "debug": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.debug = bool
        },
        "excludehidden": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.includeHidden = !bool
        },
        "firstmatch": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.firstMatch = bool
        },
        "followsymlinks": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.followSymlinks = bool
        },
        "help": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.printUsage = bool
        },
        "includehidden": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.includeHidden = bool
        },
        "multilinesearch": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.multiLineSearch = bool
        },
        "nocolorize": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.colorize = !bool
        },
        "nofollowsymlinks": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.followSymlinks = !bool
        },
        "noprintdirs": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.printDirs = !bool
        },
        "noprintfiles": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.printFiles = !bool
        },
        "noprintlines": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.printLines = !bool
        },
        "noprintmatches": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.printResults = !bool
        },
        "norecursive": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.recursive = !bool
        },
        "nosearcharchives": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.searchArchives = !bool
        },
        "printdirs": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.printDirs = bool
        },
        "printfiles": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.printFiles = bool
        },
        "printlines": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.printLines = bool
        },
        "printmatches": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.printResults = bool
        },
        "recursive": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.recursive = bool
        },
        "searcharchives": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.searchArchives = bool
        },
        "sort-ascending": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.sortDescending = !bool
        },
        "sort-caseinsensitive": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.sortCaseInsensitive = bool
        },
        "sort-casesensitive": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.sortCaseInsensitive = !bool
        },
        "sort-descending": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.sortDescending = bool
        },
        "uniquelines": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.uniqueLines = bool
        },
        "verbose": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.verbose = bool
        },
        "version": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.printVersion = bool
        }
    ]

    private var stringActionDict: [String: (String, SearchSettings) -> Void] = [
        "encoding": { (str: String, settings: SearchSettings) -> Void in
            settings.textFileEncoding = str
        },
        "in-archiveext": { (str: String, settings: SearchSettings) -> Void in
            settings.addInArchiveExtension(str)
        },
        "in-archivefilepattern": { (str: String, settings: SearchSettings) -> Void in
            settings.addInArchiveFilePattern(str)
        },
        "in-dirpattern": { (str: String, settings: SearchSettings) -> Void in
            settings.addInDirPattern(str)
        },
        "in-ext": { (str: String, settings: SearchSettings) -> Void in
            settings.addInExtension(str)
        },
        "in-filepattern": { (str: String, settings: SearchSettings) -> Void in
            settings.addInFilePattern(str)
        },
        "in-filetype": { (str: String, settings: SearchSettings) -> Void in
            settings.addInFileType(str)
        },
        "in-linesafterpattern": { (str: String, settings: SearchSettings) -> Void in
            settings.addInLinesAfterPattern(str)
        },
        "in-linesbeforepattern": { (str: String, settings: SearchSettings) -> Void in
            settings.addInLinesBeforePattern(str)
        },
        "linesaftertopattern": { (str: String, settings: SearchSettings) -> Void in
            settings.addLinesAfterToPattern(str)
        },
        "linesafteruntilpattern": { (str: String, settings: SearchSettings) -> Void in
            settings.addLinesAfterUntilPattern(str)
        },
        "maxlastmod": { (str: String, settings: SearchSettings) -> Void in
            settings.setMaxLastModFromString(str)
        },
        "minlastmod": { (str: String, settings: SearchSettings) -> Void in
            settings.setMinSizeFromString(str)
        },
        "out-archiveext": { (str: String, settings: SearchSettings) -> Void in
            settings.addOutArchiveExtension(str)
        },
        "out-archivefilepattern": { (str: String, settings: SearchSettings) -> Void in
            settings.addOutArchiveFilePattern(str)
        },
        "out-dirpattern": { (str: String, settings: SearchSettings) -> Void in
            settings.addOutDirPattern(str)
        },
        "out-ext": { (str: String, settings: SearchSettings) -> Void in
            settings.addOutExtension(str)
        },
        "out-filepattern": { (str: String, settings: SearchSettings) -> Void in
            settings.addOutFilePattern(str)
        },
        "out-filetype": { (str: String, settings: SearchSettings) -> Void in
            settings.addOutFileType(str)
        },
        "out-linesafterpattern": { (str: String, settings: SearchSettings) -> Void in
            settings.addOutLinesAfterPattern(str)
        },
        "out-linesbeforepattern": { (str: String, settings: SearchSettings) -> Void in
            settings.addOutLinesBeforePattern(str)
        },
        "path": { (str: String, settings: SearchSettings) -> Void in
            settings.addPath(str)
        },
        "searchpattern": { (str: String, settings: SearchSettings) -> Void in
            settings.addSearchPattern(str)
        },
        "sort-by": { (str: String, settings: SearchSettings) -> Void in
            settings.setSortBy(str)
        }
    ]

    private let intActionDict: [String: (Int32, SearchSettings) -> Void] = [
        "linesafter": { (i: Int32, settings: SearchSettings) -> Void in
            settings.linesAfter = i
        },
        "linesbefore": { (i: Int32, settings: SearchSettings) -> Void in
            settings.linesBefore = i
        },
        "maxdepth": { (i: Int32, settings: SearchSettings) in
            settings.maxDepth = i
        },
        "maxlinelength": { (i: Int32, settings: SearchSettings) -> Void in
            settings.maxLineLength = i
        },
        "mindepth": { (i: Int32, settings: SearchSettings) in
            settings.minDepth = i
        }
    ]

    private let longActionDict: [String: (UInt64, SearchSettings) -> Void] = [
        "maxsize": { (l: UInt64, settings: SearchSettings) in
            settings.maxSize = l
        },
        "minsize": { (l: UInt64, settings: SearchSettings) in
            settings.minSize = l
        }
    ]

    public func updateSettingsFromArgTokens(_ settings: SearchSettings, argTokens: [ArgToken]) throws {
        for argToken in argTokens {
            if argToken.type == ArgTokenType.bool {
                if let bool = argToken.value as? Bool {
                    boolActionDict[argToken.name]!(bool, settings)
                } else {
                    throw FindError(msg: "Invalid value for option: \(argToken.name)")
                }
            } else if argToken.type == ArgTokenType.str {
                if let string = argToken.value as? String {
                    if argToken.name == "settings-file" {
                        try updateSettingsFromFile(settings, filePath: string)
                    } else {
                        stringActionDict[argToken.name]!(string, settings)
                    }
                } else if let stringArray = argToken.value as? [String] {
                    for s in stringArray {
                        stringActionDict[argToken.name]!(s, settings)
                    }
                } else {
                    throw FindError(msg: "Invalid value for option: \(argToken.name)")
                }
            } else if argToken.type == ArgTokenType.int {
                if let intVal = argToken.value as? Int32 {
                    intActionDict[argToken.name]!(intVal, settings)
                } else {
                    throw FindError(msg: "Invalid value for option: \(argToken.name)")
                }
            }  else if argToken.type == ArgTokenType.long {
                if let longVal = argToken.value as? UInt64 {
                    longActionDict[argToken.name]!(longVal, settings)
                } else {
                    throw FindError(msg: "Invalid value for option: \(argToken.name)")
                }
            } else {
                throw FindError(msg: "Invalid option: \(argToken.name)")
            }
        }
    }

    public func updateSettingsFromJson(_ settings: SearchSettings, jsonString: String) throws {
        let argTokens = try argTokenizer!.tokenizeJson(jsonString)
        try updateSettingsFromArgTokens(settings, argTokens: argTokens)
    }

    public func settingsFromJson(_ jsonString: String) throws -> SearchSettings {
        let settings = SearchSettings()
        try updateSettingsFromJson(settings, jsonString: jsonString)
        return settings
    }

    public func updateSettingsFromFile(_ settings: SearchSettings, filePath: String) throws {
        let argTokens = try argTokenizer!.tokenizeFile(filePath)
        try updateSettingsFromArgTokens(settings, argTokens: argTokens)
    }

    public func settingsFromFile(_ filePath: String) throws -> SearchSettings {
        let settings = SearchSettings()
        try updateSettingsFromFile(settings, filePath: filePath)
        return settings
    }

    public func updateSettingsFromArgs(_ settings: SearchSettings, args: [String]) throws {
        let argTokens = try argTokenizer!.tokenizeArgs(args)
        try updateSettingsFromArgTokens(settings, argTokens: argTokens)
    }

    public func settingsFromArgs(_ args: [String]) throws -> SearchSettings {
        let settings = SearchSettings()
        // default printResults to true since running in cli
        settings.printResults = true
        try updateSettingsFromArgs(settings, args: args)
        return settings
    }

    func getUsageString() -> String {
        var str = "\nUsage:\n swiftsearch [options] -s <searchpattern> <path> [<path> ...]\n\n"
        str += "Options:\n"
        let options = searchOptions.sorted(by: { $0.sortArg < $1.sortArg })

        let optStrings = options.map {
            switch ($0.shortArg, $0.longArg) {
            // Order errors by code
            case let (nil, longArg):
                "--\(longArg)"
            case let ("", longArg):
                "--\(longArg)"
            case let (shortArg, longArg):
                "-\(shortArg!),--\(longArg)"
            }
        }

        let optDescs = options.map(\.desc)
        let longest = optStrings.map { $0.lengthOfBytes(using: String.Encoding.utf8) }.max()!
        for i in 0 ..< optStrings.count {
            var optLine = " \(optStrings[i])"
            while optLine.lengthOfBytes(using: String.Encoding.utf8) <= longest {
                optLine += " "
            }
            optLine += "  \(optDescs[i])\n"
            str += optLine
        }
        return str
    }

    public func usage(_ code: Int32 = 0) {
        logMsg(getUsageString())
        exit(code)
    }
}
