//
//  SearchOptions.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/17/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

struct SearchOptionStruct {
    let short: String
    let long: String
    let desc: String

    var sortArg: String {
        (short.isEmpty ? short.lowercased() + "@" : "") + long
    }
}

class SearchOption {
    let short: String
    let long: String
    let desc: String

    init(short: String, long: String, desc: String) {
        self.short = short
        self.long = long
        self.desc = desc
    }

    func sortArg() -> String {
        if !short.isEmpty {
            return short.lowercased() + "@" + long
        }
        return long
    }

    func description() -> String {
        "SearchOption(short: \"\(short)\", long: \"\(long)\", desc: \"\(desc)\")"
    }
}

class SearchOptionsXmlParser: NSObject, XMLParserDelegate {
    var searchOptions = [SearchOptionStruct]()
    let searchOptionNodeName = "searchoption"
    let longAttributeName = "long"
    let shortAttributeName = "short"
    var element = ""
    var longName = ""
    var shortName = ""
    var desc = NSMutableString()

    func parseFile(_ filepath: String) -> [SearchOptionStruct] {
        if FileManager.default.fileExists(atPath: filepath) {
            let data: Data? = try? Data(contentsOf: URL(fileURLWithPath: filepath))
            let inputStream: InputStream? = InputStream(data: data!)
            let parser: XMLParser? = XMLParser(stream: inputStream!)
            if parser != nil {
                parser!.delegate = self
                parser!.parse()
            }
        } else {
            print("ERROR: filepath not found: \(filepath)")
        }
        return searchOptions
    }

    func parser(_: XMLParser, didStartElement elementName: String,
                namespaceURI _: String?, qualifiedName _: String?,
                attributes attributeDict: [String: String])
    {
        element = elementName
        if (elementName as NSString).isEqual(to: searchOptionNodeName) {
            if attributeDict.index(forKey: longAttributeName) != nil {
                longName = (attributeDict[longAttributeName]!)
            }
            if attributeDict.index(forKey: shortAttributeName) != nil {
                shortName = (attributeDict[shortAttributeName]!)
            }
            desc = NSMutableString()
            desc = ""
        }
    }

    func parser(_: XMLParser, foundCharacters string: String) {
        if element == searchOptionNodeName {
            desc.append(string)
        }
    }

    func parser(_: XMLParser, didEndElement elementName: String,
                namespaceURI _: String?, qualifiedName _: String?)
    {
        if (elementName as NSString).isEqual(to: searchOptionNodeName) {
            if !desc.isEqual(nil) {
                let trimmedDesc = desc.trimmingCharacters(in: whitespace as CharacterSet)
                searchOptions.append(SearchOptionStruct(short: shortName,
                                                  long: longName, desc: trimmedDesc))
            }
        }
    }
}

public class SearchOptions {
    private var searchOptions = [SearchOptionStruct]()
    private var longArgDict: [String: String] = [:]

    public init() {
        setSearchOptions()
    }

    private func setSearchOptions() {
        let parser = SearchOptionsXmlParser()

//        let bundle = Bundle.main
//        let searchOptionsPath = bundle.path(forResource: "searchoptions", ofType: "xml")
//        let searchOptionsPath = bundle.path(forResource: "searchoptions", ofType: "xml", inDirectory: "Resources")
//        let searchOptionsPath = bundle.url(forResource: "searchoptions", withExtension: "xml")?.absoluteString

        searchOptions = parser.parseFile(Config.searchOptionsPath)
//        searchOptions = parser.parseFile(searchOptionsPath!)
        searchOptions.sort(by: { $0.sortArg < $1.sortArg })
        for opt in searchOptions {
            longArgDict[opt.long] = opt.long
            if !opt.short.isEmpty {
                longArgDict[opt.short] = opt.long
            }
        }
    }

    private let argActionDict: [String: (String, SearchSettings) -> Void] = [
        "encoding": { (str: String, settings: SearchSettings) -> Void in
            settings.textFileEncoding = str
        },
        "in-archiveext": { (str: String, settings: SearchSettings) -> Void in
            settings.addInArchiveExtension(str)
        },
        "in-archivefilepattern": { (str: String, settings: SearchSettings) -> Void in
            settings.inArchiveFilePatterns.append(Regex(str))
        },
        "in-dirpattern": { (str: String, settings: SearchSettings) -> Void in
            settings.inDirPatterns.append(Regex(str))
        },
        "in-ext": { (str: String, settings: SearchSettings) -> Void in
            settings.addInExtension(str)
        },
        "in-filepattern": { (str: String, settings: SearchSettings) -> Void in
            settings.inFilePatterns.append(Regex(str))
        },
        "in-filetype": { (str: String, settings: SearchSettings) -> Void in
            settings.inFileTypes.append(FileTypes.fromName(str))
        },
        "in-linesafterpattern": { (str: String, settings: SearchSettings) -> Void in
            settings.inLinesAfterPatterns.append(Regex(str))
        },
        "in-linesbeforepattern": { (str: String, settings: SearchSettings) -> Void in
            settings.inLinesBeforePatterns.append(Regex(str))
        },
        "linesafter": { (str: String, settings: SearchSettings) -> Void in
            settings.linesAfter = Int(str)!
        },
        "linesaftertopattern": { (str: String, settings: SearchSettings) -> Void in
            settings.linesAfterToPatterns.append(Regex(str))
        },
        "linesafteruntilpattern": { (str: String, settings: SearchSettings) -> Void in
            settings.linesAfterUntilPatterns.append(Regex(str))
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
            settings.outArchiveFilePatterns.append(Regex(str))
        },
        "out-dirpattern": { (str: String, settings: SearchSettings) -> Void in
            settings.outDirPatterns.append(Regex(str))
        },
        "out-ext": { (str: String, settings: SearchSettings) -> Void in
            settings.addOutExtension(str)
        },
        "out-filepattern": { (str: String, settings: SearchSettings) -> Void in
            settings.outFilePatterns.append(Regex(str))
        },
        "out-filetype": { (str: String, settings: SearchSettings) -> Void in
            settings.outFileTypes.append(FileTypes.fromName(str))
        },
        "out-linesafterpattern": { (str: String, settings: SearchSettings) -> Void in
            settings.outLinesAfterPatterns.append(Regex(str))
        },
        "out-linesbeforepattern": { (str: String, settings: SearchSettings) -> Void in
            settings.outLinesBeforePatterns.append(Regex(str))
        },
        "searchpattern": { (str: String, settings: SearchSettings) -> Void in
            settings.searchPatterns.append(Regex(str))
        },
    ]

    private let boolFlagActionDict: [String: (Bool, SearchSettings) -> Void] = [
        "allmatches": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.firstMatch = !bool
        },
        "archivesonly": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.setArchivesOnly(bool)
        },
        "colorize": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.colorize = bool
        },
        "debug": { (bool: Bool, settings: SearchSettings) -> Void in
            settings.setDebug(bool)
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

    public func settingsFromArgs(_ args: [String], error: NSErrorPointer) -> SearchSettings {
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
                            setError(error, msg: "Missing argument for option \(arg)")
                        }
                    } else if boolFlagActionDict.index(forKey: longArg!) != nil {
                        boolFlagActionDict[longArg!]!(true, settings)
                    } else {
                        setError(error, msg: "Invalid option: \(arg)")
                    }
                } else {
                    setError(error, msg: "Invalid option: \(arg)")
                }
            } else {
                settings.startPath = args[i]
            }
            i += 1
        }
        return settings
    }

    public func usage(_ code: Int32 = 0) {
        logMsg(getUsageString())
        exit(code)
    }

    func getUsageString() -> String {
        var str = "\nUsage:\n swiftsearch [options] -s <searchpattern> <startpath>\n\n"
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