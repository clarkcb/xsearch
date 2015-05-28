//
//  SearchOptions.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/17/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

class SearchOption: Printable {
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
            return short.lowercaseString + "@" + long
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

class SearchOptionsXmlParser: NSObject, NSXMLParserDelegate {
    var searchOptions = [SearchOption]()
    let searchOptionNodeName = "searchoption"
    let longAttributeName = "long"
    let shortAttributeName = "short"
    var element = ""
    var longName = ""
    var shortName = ""
    var desc = NSMutableString()

    func parseFile(filepath: String) -> [SearchOption] {
        if (NSFileManager.defaultManager().fileExistsAtPath(filepath)) {
            let data: NSData? = NSData(contentsOfFile: filepath)
            let inputStream: NSInputStream? = NSInputStream(data: data!)
            let parser: NSXMLParser? = NSXMLParser(stream: inputStream!)
            if parser != nil {
                parser!.delegate = self
                parser!.parse()
            }
        } else {
            println("ERROR: filepath not found: \(filepath)")
        }
        return searchOptions
    }

    func parser(parser: NSXMLParser, didStartElement elementName: String,
        namespaceURI: String?, qualifiedName qName: String?,
        attributes attributeDict: [NSObject : AnyObject]) {
        element = elementName
        if (elementName as NSString).isEqualToString(searchOptionNodeName) {
            if attributeDict.indexForKey(longAttributeName) != nil {
                longName = (attributeDict[longAttributeName] as! String)
            }
            if attributeDict.indexForKey(shortAttributeName) != nil {
                shortName = (attributeDict[shortAttributeName] as! String)
            }
            desc = NSMutableString.alloc()
            desc = ""
        }
    }

    func parser(parser: NSXMLParser, foundCharacters string: String?) {
        if element == searchOptionNodeName {
            desc.appendString(string!)
        }
    }

    func parser(parser: NSXMLParser, didEndElement elementName: String,
        namespaceURI: String?, qualifiedName qName: String?) {
        if (elementName as NSString).isEqualToString(searchOptionNodeName) {
            if !desc.isEqual(nil) {
                let trimmedDesc = desc.stringByTrimmingCharactersInSet(whitespace)
                searchOptions.append(SearchOption(short: shortName,
                    long: longName, desc: trimmedDesc))
            }
        }
    }
}

public class SearchOptions {
    private var searchOptions = [SearchOption]()

    init() {
        setSearchOptions()
    }

    private func setSearchOptions() {
        var parser = SearchOptionsXmlParser()
        searchOptions = parser.parseFile(Config.searchOptionsPath)
        searchOptions.sort({ $0.sortArg() < $1.sortArg() })
    }

    private let argActionDict: [String: (String, SearchSettings) -> ()] = [
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
        "in-linesafterpattern": { (s: String, ss: SearchSettings) -> () in
            ss.inLinesAfterPatterns.append(Regex(s))
        },
        "in-linesbeforepattern": { (s: String, ss: SearchSettings) -> () in
            ss.inLinesBeforePatterns.append(Regex(s))
        },
        "linesafter": { (s: String, ss: SearchSettings) -> () in
            ss.linesAfter = s.toInt()!
        },
        "linesaftertopattern": { (s: String, ss: SearchSettings) -> () in
            ss.linesAfterToPatterns.append(Regex(s))
        },
        "linesafteruntilpattern": { (s: String, ss: SearchSettings) -> () in
            ss.linesAfterUntilPatterns.append(Regex(s))
        },
        "linesbefore": { (s: String, ss: SearchSettings) -> () in
            ss.linesBefore = s.toInt()!
        },
        "maxlinelength": { (s: String, ss: SearchSettings) -> () in
            ss.maxLineLength = s.toInt()!
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

    private let flagActionDict: [String: SearchSettings -> ()] = [
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
        "dotiming": { (ss: SearchSettings) -> () in
            ss.doTiming = true
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

    private func dictFromOptions(options: [SearchOption]) -> [String:SearchOption] {
        var dict = toDictionary(options) {($0.long, $0)}
        for (k, v) in (toDictionary(options.filter {!$0.short.isEmpty}) {($0.short, $0)}) {
            dict[k] = v
        }
        return dict
    }

    func settingsFromArgs(args: [String]) -> SearchSettings {
        var i = 0
        let settings = SearchSettings()
        let argDict = dictFromOptions(searchOptions.filter
            {self.argActionDict.indexForKey($0.long) != nil})
        let flagDict = dictFromOptions(searchOptions.filter
            {self.flagActionDict.indexForKey($0.long) != nil})
        while i < args.count {
            var arg = args[i]
            if arg.hasPrefix("-") {
                while arg.hasPrefix("-") && count(arg) > 1 {
                    arg = arg.substringFromIndex(advance(arg.startIndex, 1))
                }
                if argDict.indexForKey(arg) != nil {
                    if args.count > i {
                        argActionDict[argDict[arg]!.long]!(args[i+1], settings)
                        i++
                    } else {
                        println("ERROR: missing argument for option \(arg)")
                    }
                } else if flagDict.indexForKey(arg) != nil {
                    flagActionDict[flagDict[arg]!.long]!(settings)
                } else {
                    println("ERROR: unknown arg: \(args[i])")
                }
            } else {
                settings.startPath = args[i]
            }
            i++
        }
        return settings
    }

    func usage(code: Int32 = 0) {
        logMsg(getUsageString())
        exit(code)
    }

    func getUsageString() -> String {
        var s = "\nUsage:\n swiftsearch [options] -s <searchpattern> <startpath>\n\n"
        s += "Options:\n"
        let optStrings = searchOptions.map
            { $0.short.isEmpty ? "--\($0.long)" : "-\($0.short),--\($0.long)" }
        let optDescs = searchOptions.map { $0.desc }
        let longest = maxElement(optStrings.map({ count($0) }))
        for var i=0; i < count(optStrings); ++i {
            var optLine = " \(optStrings[i])"
            while count(optLine) <= longest {
                optLine += " "
            }
            optLine += "  \(optDescs[i])\n"
            s += optLine
        }
        return s
    }
}
