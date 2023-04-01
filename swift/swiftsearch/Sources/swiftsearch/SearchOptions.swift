//
//  SearchOptions.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/17/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

struct SearchOption {
    let short: String
    let long: String
    let desc: String

    var sortArg: String {
        (short.isEmpty ? short.lowercased() + "@" : "") + long
    }
}

public class SearchOptions {
    private var config: Config
    private var searchOptions = [SearchOption]()
    private var longArgDict: [String: String] = [:]

    public init() {
        self.config = Config()
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
                        searchOptions.append(SearchOption(short: shortArg, long: longArg, desc: desc))
                    }
                    searchOptions.sort(by: { $0.sortArg < $1.sortArg })
                    for opt in searchOptions {
                        longArgDict[opt.long] = opt.long
                        if !opt.short.isEmpty {
                            longArgDict[opt.short] = opt.long
                        }
                    }
                }
            }
        } catch let error as NSError {
            print("Failed to load: \(error.localizedDescription)")
        }
    }

    // this is computed property so that it can reference self
    private var argActionDict: [String: (String, SearchSettings) -> Void] {
        [
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
            "linesafter": { (str: String, settings: SearchSettings) -> Void in
                settings.linesAfter = Int(str)!
            },
            "linesaftertopattern": { (str: String, settings: SearchSettings) -> Void in
                settings.addLinesAfterToPattern(str)
            },
            "linesafteruntilpattern": { (str: String, settings: SearchSettings) -> Void in
                settings.addLinesAfterUntilPattern(str)
            },
            "linesbefore": { (str: String, settings: SearchSettings) -> Void in
                settings.linesBefore = Int(str)!
            },
            "maxlinelength": { (str: String, settings: SearchSettings) -> Void in
                settings.maxLineLength = Int(str)!
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
            "settings-file": { (str: String, settings: SearchSettings) -> Void in
                try? self.addSettingsFromFile(str, settings: settings)
            },
        ]
    }

    private let boolFlagActionDict: [String: (Bool, SearchSettings) -> Void] = [
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
            settings.excludeHidden = bool
        },
        "firstmatch": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.firstMatch = bool
        },
        "help": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.printUsage = bool
        },
        "includehidden": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.excludeHidden = !bool
        },
        "listdirs": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.listDirs = bool
        },
        "listfiles": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.listFiles = bool
        },
        "listlines": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.listLines = bool
        },
        "multilinesearch": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.multiLineSearch = bool
        },
        "nocolorize": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.colorize = !bool
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
        "printmatches": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.printResults = bool
        },
        "recursive": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.recursive = bool
        },
        "searcharchives": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.searchArchives = bool
        },
        "uniquelines": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.uniqueLines = bool
        },
        "verbose": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.verbose = bool
        },
        "version": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.printVersion = bool
        },
    ]

    public func settingsFromArgs(_ args: [String]) throws -> SearchSettings {
        var i = 0
        let settings = SearchSettings()
        while i < args.count {
            var arg = args[i]
            if arg.hasPrefix("-") {
                while arg.hasPrefix("-"), arg.lengthOfBytes(using: String.Encoding.utf8) > 1 {
                    arg = String(arg[arg.index(arg.startIndex, offsetBy: 1)...])
                }
                if longArgDict.index(forKey: arg) != nil {
                    let longArg = longArgDict[arg]
                    if argActionDict.index(forKey: longArg!) != nil {
                        if args.count > i + 1 {
                            argActionDict[longArg!]!(args[i + 1], settings)
                            i += 1
                        } else {
                            throw SearchError(msg: "Missing argument for option \(arg)")
                        }
                    } else if boolFlagActionDict.index(forKey: longArg!) != nil {
                        boolFlagActionDict[longArg!]!(true, settings)
                    } else {
                        throw SearchError(msg: "Invalid option: \(arg)")
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

    public func settingsFromFile(_ filePath: String) throws -> SearchSettings {
        let settings = SearchSettings()
        try addSettingsFromFile(filePath, settings: settings)
        return settings
    }

    public func addSettingsFromFile(_ filePath: String, settings: SearchSettings) throws {
        do {
            let fileUrl = URL(fileURLWithPath: filePath)
            let jsonString = try String(contentsOf: fileUrl, encoding: .utf8)
            try addSettingsFromJson(jsonString, settings: settings)
        } catch let error as NSError {
            print("Failed to load: \(error.localizedDescription)")
        }
    }

    public func settingsFromJson(_ jsonString: String) throws -> SearchSettings {
        let settings = SearchSettings()
        try addSettingsFromJson(jsonString, settings: settings)
        return settings
    }

    public func addSettingsFromJson(_ jsonString: String, settings: SearchSettings) throws {
        do {
            if let json = try JSONSerialization.jsonObject(with: jsonString.data(using: .utf8)!,
                                                           options: []) as? [String: Any]
            {
                for key in json.keys {
                    if longArgDict.index(forKey: key) != nil {
                        let longArg = longArgDict[key]
                        if argActionDict.index(forKey: longArg!) != nil {
                            let value = json[key]
                            if let string = value as? String {
                                argActionDict[longArg!]!(string, settings)
                            } else if let bool = value as? Bool {
                                argActionDict[longArg!]!(bool.description, settings)
                            } else if let int = value as? Int {
                                argActionDict[longArg!]!(int.description, settings)
                            } else if let stringArray = value as? [String] {
                                for s in stringArray {
                                    argActionDict[longArg!]!(s, settings)
                                }
                            } else {
                                throw SearchError(msg: "Invalid type for \"\(key)\" entry")
                            }
                        } else if boolFlagActionDict.index(forKey: longArg!) != nil {
                            let value = json[key]
                            if let bool = value as? Bool {
                                boolFlagActionDict[longArg!]!(bool, settings)
                            } else {
                                throw SearchError(msg: "Invalid type for \"\(key)\" entry")
                            }
                        } else {
                            throw SearchError(msg: "Invalid option: \(key)")
                        }
                    } else if key == "path" {
                        let value = json[key]
                        if let string = value as? String {
                            settings.addPath(string)
                        }
                    } else {
                        throw SearchError(msg: "Invalid option: \(key)")
                    }
                }
            }
        } catch let error as NSError {
            print("Failed to load: \(error.localizedDescription)")
        }
    }

    public func usage(_ code: Int32 = 0) {
        logMsg(getUsageString())
        exit(code)
    }

    func getUsageString() -> String {
        var str = "\nUsage:\n swiftsearch [options] -s <searchpattern> <path> [<path> ...]\n\n"
        str += "Options:\n"
        let optStrings = searchOptions.map {
            $0.short.isEmpty ? "--\($0.long)" : "-\($0.short),--\($0.long)"
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
}
