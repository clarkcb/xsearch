//
//  FileTypes.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public enum FileType {
    case Unknown, Archive, Binary, Text
}

class FileTypesXmlParser: NSObject, NSXMLParserDelegate {
    var fileTypeDict: [String:Set<String>] = [:]
    let fileTypeNodeName = "filetype"
    let extensionsNodeName = "extensions"
    let nameAttributeName = "name"
    var element = ""
    var fileTypeName = ""
    var extensions = NSMutableString()

    func parseFile(filepath: String) -> [String: Set<String>] {
        if (NSFileManager.defaultManager().fileExistsAtPath(filepath)) {
            let data: NSData? = NSData(contentsOfFile: filepath)
            let inputStream: NSInputStream? = NSInputStream(data: data!)

            let parser: NSXMLParser? = NSXMLParser(stream: inputStream!)
            if parser != nil {
                parser!.delegate = self
                parser!.parse()
            }
        } else {
            print("ERROR: filepath not found: \(filepath)")
        }

        return fileTypeDict
    }

    func parser(parser: NSXMLParser, didStartElement elementName: String,
        namespaceURI: String?, qualifiedName qName: String?,
        attributes attributeDict: [String : String]) {
        element = elementName
        if (elementName as NSString).isEqualToString(fileTypeNodeName) {
            if attributeDict.indexForKey(nameAttributeName) != nil {
                fileTypeName = (attributeDict[nameAttributeName]!)
            }
            extensions = NSMutableString()
            extensions = ""
        }
    }

    func parser(parser: NSXMLParser, foundCharacters string: String) {
        if element == extensionsNodeName {
            extensions.appendString(string)
        }
    }

    func parser(parser: NSXMLParser, didEndElement elementName: String,
        namespaceURI: String?, qualifiedName qName: String?) {
        if (elementName as NSString).isEqualToString(fileTypeNodeName) {
            if !extensions.isEqual(nil) {
                let xs = extensions.componentsSeparatedByCharactersInSet(whitespace)
                fileTypeDict[fileTypeName] = Set(xs )
            }
        }
    }
}

public class FileTypes {
    private var fileTypesDict = [String: Set<String>]()
    private let archive = "archive"
    private let binary = "binary"
    private let searchable = "searchable"
    private let text = "text"
    private let unknown = "unknown"

    init() {
        setFileTypeDict()
    }

    private func setFileTypeDict() {
        let parser = FileTypesXmlParser()
        fileTypesDict = parser.parseFile(Config.fileTypesPath)
        fileTypesDict[text] = fileTypesDict[text]!.union(fileTypesDict["code"]!)
            .union(fileTypesDict["xml"]!)
        fileTypesDict[searchable] =
            fileTypesDict[text]!.union(fileTypesDict[binary]!)
                .union(fileTypesDict[archive]!)
    }

    public func getFileType(fileName: String) -> FileType {
        if isTextFile(fileName) {
            return FileType.Text
        }
        if isBinaryFile(fileName) {
            return FileType.Binary
        }
        if isArchiveFile(fileName) {
            return FileType.Archive
        }
        return FileType.Unknown
    }

    public func isArchiveFile(fileName: String) -> Bool {
        return fileTypesDict.indexForKey(archive) != nil &&
            fileTypesDict[archive]!.contains(FileUtil.getExtension(fileName))
    }

    public func isBinaryFile(fileName: String) -> Bool {
        return fileTypesDict.indexForKey(binary) != nil &&
            fileTypesDict[binary]!.contains(FileUtil.getExtension(fileName))
    }

    public func isSearchableFile(fileName: String) -> Bool {
        return fileTypesDict.indexForKey(searchable) != nil &&
            fileTypesDict[searchable]!.contains(FileUtil.getExtension(fileName))
    }

    public func isTextFile(fileName: String) -> Bool {
        return fileTypesDict.indexForKey(text) != nil &&
            fileTypesDict[text]!.contains(FileUtil.getExtension(fileName))
    }

    public func isUnknownFile(fileName: String) -> Bool {
        return (fileTypesDict.indexForKey(unknown) != nil &&
            fileTypesDict[unknown]!.contains(FileUtil.getExtension(fileName)))
            || !isSearchableFile(fileName)
    }
}
