//
//  SearchOptions.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/17/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

class SearchOption: CustomStringConvertible {
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

    var description: String {
        var s = "SearchOption("
        s += "short: \"\(short)\""
        s += ", long: \"\(long)\""
        s += ", desc: \"\(desc)\""
        s += ")"
        return s
    }
}

class SearchOptionsXmlParser: NSObject, XMLParserDelegate {
    var searchOptions = [SearchOption]()
    let searchOptionNodeName = "searchoption"
    let longAttributeName = "long"
    let shortAttributeName = "short"
    var element = ""
    var longName = ""
    var shortName = ""
    var desc = NSMutableString()

    func parseFile(_ filepath: String) -> [SearchOption] {
        if (FileManager.default.fileExists(atPath: filepath)) {
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

    func parser(_ parser: XMLParser, didStartElement elementName: String,
        namespaceURI: String?, qualifiedName qName: String?,
        attributes attributeDict: [String : String]) {
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

    func parser(_ parser: XMLParser, foundCharacters string: String) {
        if element == searchOptionNodeName {
            desc.append(string)
        }
    }

    func parser(_ parser: XMLParser, didEndElement elementName: String,
        namespaceURI: String?, qualifiedName qName: String?) {
        if (elementName as NSString).isEqual(to: searchOptionNodeName) {
            if !desc.isEqual(nil) {
                let trimmedDesc = desc.trimmingCharacters(in: whitespace as CharacterSet)
                searchOptions.append(SearchOption(short: shortName,
                    long: longName, desc: trimmedDesc))
            }
        }
    }
}

open class SearchOptions {
    fileprivate var searchOptions = [SearchOption]()
    fileprivate var longArgDict: [String:String] = [:]

    init() {
        setSearchOptions()
    }

    fileprivate func setSearchOptions() {
        let parser = SearchOptionsXmlParser()
        searchOptions = parser.parseFile(Config.searchOptionsPath)
        searchOptions.sort(by: { $0.sortArg() < $1.sortArg() })
        for so in searchOptions {
            longArgDict[so.long] = so.long
            if !so.short.isEmpty {
                longArgDict[so.short] = so.long
            }
        }
    }

    fileprivate let argActionDict: [String: (String, SearchSettings) -> ()] = [
        "in-archiveext": { (s: String, ss: SearchSettings) -> () in
            ss.addInArchiveExtension(s)
        },
        "in-archivefilepattern": { (s: String, ss: SearchSettings) -> () in
            ss.inArchiveFilePatterns.append(Regex(s))
        },
        "in-dirpattern": { (s: String, ss: SearchSettings) -> () in
            ss.inDirPatterns.append(Regex(s))
        },
        "in-ext": { (s: String, ss: SearchSettings) -> () in
            ss.addInExtension(s)
        },
        "in-filepattern": { (s: String, ss: SearchSettings) -> () in
            ss.inFilePatterns.append(Regex(s))
        },
        "in-filetype": { (s: String, ss: SearchSettings) -> () in
            ss.inFilePatterns.append(Regex(s))
        },
        "in-linesafterpattern": { (s: String, ss: SearchSettings) -> () in
            ss.inLinesAfterPatterns.append(Regex(s))
        },
        "in-linesbeforepattern": { (s: String, ss: SearchSettings) -> () in
            ss.inLinesBeforePatterns.append(Regex(s))
        },
        "linesafter": { (s: String, ss: SearchSettings) -> () in
            ss.linesAfter = Int(s)!
        },
        "linesaftertopattern": { (s: String, ss: SearchSettings) -> () in
            ss.linesAfterToPatterns.append(Regex(s))
        },
        "linesafteruntilpattern": { (s: String, ss: SearchSettings) -> () in
            ss.linesAfterUntilPatterns.append(Regex(s))
        },
        "linesbefore": { (s: String, ss: SearchSettings) -> () in
            ss.linesBefore = Int(s)!
        },
        "maxlinelength": { (s: String, ss: SearchSettings) -> () in
            ss.maxLineLength = Int(s)!
        },
        "out-archiveext": { (s: String, ss: SearchSettings) -> () in
            ss.addOutArchiveExtension(s)
        },
        "out-archivefilepattern": { (s: String, ss: SearchSettings) -> () in
            ss.outArchiveFilePatterns.append(Regex(s))
        },
        "out-dirpattern": { (s: String, ss: SearchSettings) -> () in
            ss.outDirPatterns.append(Regex(s))
        },
        "out-ext": { (s: String, ss: SearchSettings) -> () in
            ss.addOutExtension(s)
        },
        "out-filepattern": { (s: String, ss: SearchSettings) -> () in
            ss.outFilePatterns.append(Regex(s))
        },
        "out-filetype": { (s: String, ss: SearchSettings) -> () in
            ss.outFilePatterns.append(Regex(s))
        },
        "out-linesafterpattern": { (s: String, ss: SearchSettings) -> () in
            ss.outLinesAfterPatterns.append(Regex(s))
        },
        "out-linesbeforepattern": { (s: String, ss: SearchSettings) -> () in
            ss.outLinesBeforePatterns.append(Regex(s))
        },
        "search": { (s: String, ss: SearchSettings) -> () in
            ss.searchPatterns.append(Regex(s))
        }
    ]

    fileprivate let boolFlagActionDict: [String: (Bool, SearchSettings) -> ()] = [
        "allmatches": { (b: Bool, ss: SearchSettings) -> () in
            ss.firstMatch = !b
        },
        "archivesonly": { (b: Bool, ss: SearchSettings) -> () in
            ss.archivesOnly = b
            if b { ss.searchArchives = true }
        },
        "debug": { (b: Bool, ss: SearchSettings) -> () in
            ss.debug = b
            if b { ss.verbose = true }
        },
        "excludehidden": { (b: Bool, ss: SearchSettings) -> () in
            ss.excludeHidden = b
        },
        "firstmatch": { (b: Bool, ss: SearchSettings) -> () in
            ss.firstMatch = b
        },
        "help": { (b: Bool, ss: SearchSettings) -> () in
            ss.printUsage = b
        },
        "includehidden": { (b: Bool, ss: SearchSettings) -> () in
            ss.excludeHidden = !b
        },
        "listdirs": { (b: Bool, ss: SearchSettings) -> () in
            ss.listDirs = b
        },
        "listfiles": { (b: Bool, ss: SearchSettings) -> () in
            ss.listFiles = b
        },
        "listlines": { (b: Bool, ss: SearchSettings) -> () in
            ss.listLines = b
        },
        "multilinesearch": { (b: Bool, ss: SearchSettings) -> () in
            ss.multiLineSearch = b
        },
        "noprintmatches": { (b: Bool, ss: SearchSettings) -> () in
            ss.printResults = !b
        },
        "norecursive": { (b: Bool, ss: SearchSettings) -> () in
            ss.recursive = !b
        },
        "nosearcharchives": { (b: Bool, ss: SearchSettings) -> () in
            ss.searchArchives = !b
        },
        "printmatches": { (b: Bool, ss: SearchSettings) -> () in
            ss.printResults = b
        },
        "recursive": { (b: Bool, ss: SearchSettings) -> () in
            ss.recursive = b
        },
        "searcharchives": { (b: Bool, ss: SearchSettings) -> () in
            ss.searchArchives = b
        },
        "uniquelines": { (b: Bool, ss: SearchSettings) -> () in
            ss.uniqueLines = b
        },
        "verbose": { (b: Bool, ss: SearchSettings) -> () in
            ss.verbose = b
        },
        "version": { (b: Bool, ss: SearchSettings) -> () in
            ss.printVersion = b
        }
    ]

    func settingsFromArgs(_ args: [String], error: NSErrorPointer) -> SearchSettings {
        var i = 0
        let settings = SearchSettings()
        while i < args.count {
            var arg = args[i]
            if arg.hasPrefix("-") {
                while arg.hasPrefix("-") && arg.lengthOfBytes(using: String.Encoding.utf8) > 1 {
                    arg = String(arg[arg.index(arg.startIndex, offsetBy: 1)...])
                }
                if longArgDict.index(forKey: arg) != nil {
                    let longArg = longArgDict[arg]
                    if argActionDict.index(forKey: longArg!) != nil {
                        if args.count > i+1 {
                            argActionDict[longArg!]!(args[i+1], settings)
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

    func usage(_ code: Int32 = 0) {
        logMsg(getUsageString())
        exit(code)
    }

    func getUsageString() -> String {
        var s = "\nUsage:\n swiftsearch [options] -s <searchpattern> <startpath>\n\n"
        s += "Options:\n"
        let optStrings = searchOptions.map
            { $0.short.isEmpty ? "--\($0.long)" : "-\($0.short),--\($0.long)" }
        let optDescs = searchOptions.map { $0.desc }
        let longest = optStrings.map({ $0.lengthOfBytes(using: String.Encoding.utf8) }).max()!
        for i in 0 ..< optStrings.count {
            var optLine = " \(optStrings[i])"
            while optLine.lengthOfBytes(using: String.Encoding.utf8) <= longest {
                optLine += " "
            }
            optLine += "  \(optDescs[i])\n"
            s += optLine
        }
        return s
    }
}
