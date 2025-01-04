//
//  SearchOptions.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/17/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation
import swiftfind

struct SearchOption {
    let shortArg: String?
    let longArg: String
    let desc: String

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
    // Add path here because it isn't included in findoptions.json
    private var longArgDict: [String: String] = ["path": "path"]

    public init() {
        self.config = SearchConfig()
        setSearchOptionsFromJson()
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
                        searchOptions.append(SearchOption(shortArg: shortArg, longArg: longArg, desc: desc))
                    }
                    // searchOptions.sort(by: { $0.sortArg < $1.sortArg })
                    for opt in searchOptions {
                        longArgDict[opt.longArg] = opt.longArg
                        if opt.shortArg != nil, !opt.shortArg!.isEmpty {
                            longArgDict[opt.shortArg!] = opt.longArg
                        }
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

    public func updateSettingsFromJson(_ jsonString: String, settings: SearchSettings) throws {
        do {
            if let json = try JSONSerialization.jsonObject(with: jsonString.data(using: .utf8)!,
                                                           options: []) as? [String: Any]
            {
                // keys are sorted so that output is consistent across all versions
                let keys = json.keys.sorted()
                let invalidKeys = keys.filter { longArgDict.index(forKey: $0) == nil }
                if !invalidKeys.isEmpty {
                    throw SearchError(msg: "Invalid option: \(invalidKeys[0])")
                }
                for key in keys {
                    if longArgDict.index(forKey: key) != nil {
                        let longArg = longArgDict[key]
                        if boolActionDict.index(forKey: longArg!) != nil {
                            let value = json[key]
                            if let bool = value as? Bool {
                                boolActionDict[longArg!]!(bool, settings)
                            } else {
                                throw SearchError(msg: "Invalid value for option: \(key)")
                            }
                        } else if stringActionDict.index(forKey: longArg!) != nil {
                            let value = json[key]
                            if let string = value as? String {
                                stringActionDict[longArg!]!(string, settings)
                            } else if let stringArray = value as? [String] {
                                for s in stringArray {
                                    stringActionDict[longArg!]!(s, settings)
                                }
                            } else {
                                throw SearchError(msg: "Invalid value for option: \(key)")
                            }
                        } else if intActionDict.index(forKey: longArg!) != nil {
                            let value = json[key]
                            if let intVal = value as? Int32 {
                                intActionDict[longArg!]!(intVal, settings)
                            } else {
                                throw SearchError(msg: "Invalid value for option: \(key)")
                            }
                        } else if longActionDict.index(forKey: longArg!) != nil {
                            let value = json[key]
                            if let longVal = value as? UInt64 {
                                longActionDict[longArg!]!(longVal, settings)
                            } else {
                                throw SearchError(msg: "Invalid value for option: \(key)")
                            }
                        } else {
                            throw SearchError(msg: "Invalid option: \(key)")
                        }
                    } else {
                        throw SearchError(msg: "Invalid option: \(key)")
                    }
                }
            }
        } catch let error as SearchError {
            throw error
        } catch let error {
            throw SearchError(msg: "Failed to load: \(error.localizedDescription)")
        }
    }

    public func settingsFromJson(_ jsonString: String) throws -> SearchSettings {
        let settings = SearchSettings()
        try updateSettingsFromJson(jsonString, settings: settings)
        return settings
    }

    public func updateSettingsFromFile(_ filePath: String, settings: SearchSettings) throws {
        let expandedPath = FileUtil.expandPath(filePath)
        if !FileUtil.exists(expandedPath) {
            throw SearchError(msg: "Settings file not found: \(filePath)")
        }
        if !expandedPath.hasSuffix(".json") {
            throw SearchError(msg: "Invalid settings file (must be JSON): \(filePath)")
        }
        do {
            let fileUrl = URL(fileURLWithPath: filePath)
            let jsonString = try String(contentsOf: fileUrl, encoding: .utf8)
            try updateSettingsFromJson(jsonString, settings: settings)
        } catch let error as SearchError {
            throw error
        } catch let error {
            throw SearchError(msg: "Failed to load: \(error.localizedDescription)")
        }
    }

    public func settingsFromFile(_ filePath: String) throws -> SearchSettings {
        let settings = SearchSettings()
        try updateSettingsFromFile(filePath, settings: settings)
        return settings
    }

    public func settingsFromArgs(_ args: [String]) throws -> SearchSettings {
        var i = 0
        let settings = SearchSettings()
        // default printResults to true since running as cli
        settings.printResults = true
        while i < args.count {
            var arg = args[i]
            if arg.hasPrefix("-") {
                while arg.hasPrefix("-"), arg.lengthOfBytes(using: String.Encoding.utf8) > 1 {
                    arg = String(arg[arg.index(arg.startIndex, offsetBy: 1)...])
                }
                if longArgDict.index(forKey: arg) != nil {
                    let longArg = longArgDict[arg]
                    if boolActionDict.index(forKey: longArg!) != nil {
                        boolActionDict[longArg!]!(true, settings)
                    } else {
                        var argVal = ""
                        if args.count > i + 1 {
                            i += 1
                            argVal = args[i]
                        } else {
                            throw SearchError(msg: "Missing argument for option \(arg)")
                        }
                        
                        if stringActionDict.index(forKey: longArg!) != nil {
                            stringActionDict[longArg!]!(argVal, settings)
                        } else if intActionDict.index(forKey: longArg!) != nil {
                            let intVal = Int32(argVal) ?? 0
                            intActionDict[longArg!]!(intVal, settings)
                        } else if longActionDict.index(forKey: longArg!) != nil {
                            let longVal = UInt64(argVal) ?? 0
                            longActionDict[longArg!]!(longVal, settings)
                        } else if longArg == "settings-file" {
                            do {
                                try updateSettingsFromFile(argVal, settings: settings)
                            } catch let err as SearchError {
                                throw err
                            }
                        } else {
                            throw SearchError(msg: "Invalid option: \(arg)")
                        }
                    }
                } else {
                    throw SearchError(msg: "Invalid option: \(arg)")
                }
            } else {
                settings.addPath(args[i])
            }
            i += 1
        }
        return settings
    }

    func getUsageString() -> String {
        var str = "\nUsage:\n swiftsearch [options] -s <searchpattern> <path> [<path> ...]\n\n"
        str += "Options:\n"
        //searchOptions.sort(by: { $0.sortArg < $1.sortArg })

        let optStrings = searchOptions.map {
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

        let optDescs = searchOptions.map(\.desc)
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
