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

    init() {
        setSearchOptions()
    }

    fileprivate func setSearchOptions() {
        let parser = SearchOptionsXmlParser()
        searchOptions = parser.parseFile(Config.searchOptionsPath)
        searchOptions.sort(by: { $0.sortArg() < $1.sortArg() })
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
        },
    ]

    fileprivate let flagActionDict: [String: (SearchSettings) -> ()] = [
        "allmatches": { (ss: SearchSettings) -> () in
            ss.firstMatch = false
        },
        "archivesonly": { (ss: SearchSettings) -> () in
            ss.archivesOnly = true
            ss.searchArchives = true
        },
        "debug": { (ss: SearchSettings) -> () in
            ss.debug = true
            ss.verbose = true
        },
        "excludehidden": { (ss: SearchSettings) -> () in
            ss.excludeHidden = true
        },
        "firstmatch": { (ss: SearchSettings) -> () in
            ss.firstMatch = true
        },
        "help": { (ss: SearchSettings) -> () in
            ss.printUsage = true
        },
        "includehidden": { (ss: SearchSettings) -> () in
            ss.excludeHidden = false
        },
        "listdirs": { (ss: SearchSettings) -> () in
            ss.listDirs = true
        },
        "listfiles": { (ss: SearchSettings) -> () in
            ss.listFiles = true
        },
        "listlines": { (ss: SearchSettings) -> () in
            ss.listLines = true
        },
        "multilinesearch": { (ss: SearchSettings) -> () in
            ss.multiLineSearch = true
        },
        "noprintmatches": { (ss: SearchSettings) -> () in
            ss.printResults = false
        },
        "norecursive": { (ss: SearchSettings) -> () in
            ss.recursive = false
        },
        "nosearcharchives": { (ss: SearchSettings) -> () in
            ss.searchArchives = false
        },
        "printmatches": { (ss: SearchSettings) -> () in
            ss.printResults = true
        },
        "recursive": { (ss: SearchSettings) -> () in
            ss.recursive = true
        },
        "searcharchives": { (ss: SearchSettings) -> () in
            ss.searchArchives = true
        },
        "uniquelines": { (ss: SearchSettings) -> () in
            ss.uniqueLines = true
        },
        "verbose": { (ss: SearchSettings) -> () in
            ss.verbose = true
        },
        "version": { (ss: SearchSettings) -> () in
            ss.printVersion = true
        },
    ]

    fileprivate func dictFromOptions(_ options: [SearchOption]) -> [String:SearchOption] {
        var dict = toDictionary(options) {($0.long, $0)}
        for (k, v) in (toDictionary(options.filter {!$0.short.isEmpty}) {($0.short, $0)}) {
            dict[k] = v
        }
        return dict
    }

//    init(settings: SearchSettings, error: NSErrorPointer) {
    func settingsFromArgs(_ args: [String], error: NSErrorPointer) -> SearchSettings {
        var i = 0
        let settings = SearchSettings()
        let argDict = dictFromOptions(searchOptions.filter
            {self.argActionDict.index(forKey: $0.long) != nil})
        let flagDict = dictFromOptions(searchOptions.filter
            {self.flagActionDict.index(forKey: $0.long) != nil})
        while i < args.count {
            var arg = args[i]
            if arg.hasPrefix("-") {
                while arg.hasPrefix("-") && arg.characters.count > 1 {
                    arg = arg.substring(from: arg.characters.index(arg.startIndex, offsetBy: 1))
                }
                if argDict.index(forKey: arg) != nil {
                    if args.count > i {
                        argActionDict[argDict[arg]!.long]!(args[i+1], settings)
                        i += 1
                    } else {
                        setError(error, msg: "Missing argument for option \(arg)")
                    }
                } else if flagDict.index(forKey: arg) != nil {
                    flagActionDict[flagDict[arg]!.long]!(settings)
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
        let longest = optStrings.map({ $0.characters.count }).max()!
        for i in 0 ..< optStrings.count {
            var optLine = " \(optStrings[i])"
            while optLine.characters.count <= longest {
                optLine += " "
            }
            optLine += "  \(optDescs[i])\n"
            s += optLine
        }
        return s
    }
}
