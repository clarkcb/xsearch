//
//  FileTypes.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public enum FileType {
    case unknown, archive, binary, code, text, xml
}

class FileTypesXmlParser: NSObject, XMLParserDelegate {
    var fileTypeDict: [String:Set<String>] = [:]
    let fileTypeNodeName = "filetype"
    let extensionsNodeName = "extensions"
    let nameAttributeName = "name"
    var element = ""
    var fileTypeName = ""
    var extensions = NSMutableString()

    func parseFile(_ filepath: String) -> [String: Set<String>] {
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

        return fileTypeDict
    }

    func parser(_ parser: XMLParser, didStartElement elementName: String,
        namespaceURI: String?, qualifiedName qName: String?,
        attributes attributeDict: [String : String]) {
        element = elementName
        if (elementName as NSString).isEqual(to: fileTypeNodeName) {
            if attributeDict.index(forKey: nameAttributeName) != nil {
                fileTypeName = (attributeDict[nameAttributeName]!)
            }
            extensions = NSMutableString()
            extensions = ""
        }
    }

    func parser(_ parser: XMLParser, foundCharacters string: String) {
        if element == extensionsNodeName {
            extensions.append(string)
        }
    }

    func parser(_ parser: XMLParser, didEndElement elementName: String,
        namespaceURI: String?, qualifiedName qName: String?) {
        if (elementName as NSString).isEqual(to: fileTypeNodeName) {
            if !extensions.isEqual(nil) {
                let xs = extensions.components(separatedBy: whitespace)
                fileTypeDict[fileTypeName] = Set(xs )
            }
        }
    }
}

open class FileTypes {
    static fileprivate let archive = "archive"
    static fileprivate let binary = "binary"
    static fileprivate let code = "code"
    static fileprivate let searchable = "searchable"
    static fileprivate let text = "text"
    static fileprivate let unknown = "unknown"
    static fileprivate let xml = "xml"

    fileprivate var fileTypesDict = [String: Set<String>]()

    init() {
        setFileTypeDict()
    }

    fileprivate func setFileTypeDict() {
        let parser = FileTypesXmlParser()
        fileTypesDict = parser.parseFile(Config.fileTypesPath)
        fileTypesDict[FileTypes.text] = fileTypesDict[FileTypes.text]!.union(fileTypesDict[FileTypes.code]!)
            .union(fileTypesDict[FileTypes.xml]!)
        fileTypesDict[FileTypes.searchable] =
            fileTypesDict[FileTypes.text]!.union(fileTypesDict[FileTypes.binary]!)
                .union(fileTypesDict[FileTypes.archive]!)
    }
    
    static open func fromName(_ typeName: String) -> FileType {
        let lname = typeName.lowercased()
        if lname == text {
            return FileType.text
        }
        if lname == binary {
            return FileType.binary
        }
        if lname == archive {
            return FileType.archive
        }
        if lname == code {
            return FileType.code
        }
        if lname == xml {
            return FileType.xml
        }
        return FileType.unknown
    }
    
    open func getFileType(_ fileName: String) -> FileType {
        if isTextFile(fileName) {
            return FileType.text
        }
        if isBinaryFile(fileName) {
            return FileType.binary
        }
        if isArchiveFile(fileName) {
            return FileType.archive
        }
        if isCodeFile(fileName) {
            return FileType.code
        }
        if isXmlFile(fileName) {
            return FileType.xml
        }
        return FileType.unknown
    }

    open func isArchiveFile(_ fileName: String) -> Bool {
        return fileTypesDict.index(forKey: FileTypes.archive) != nil &&
            fileTypesDict[FileTypes.archive]!.contains(FileUtil.getExtension(fileName))
    }

    open func isBinaryFile(_ fileName: String) -> Bool {
        return fileTypesDict.index(forKey: FileTypes.binary) != nil &&
            fileTypesDict[FileTypes.binary]!.contains(FileUtil.getExtension(fileName))
    }
    
    open func isCodeFile(_ fileName: String) -> Bool {
        return fileTypesDict.index(forKey: FileTypes.code) != nil &&
            fileTypesDict[FileTypes.code]!.contains(FileUtil.getExtension(fileName))
    }
    
    open func isSearchableFile(_ fileName: String) -> Bool {
        return fileTypesDict.index(forKey: FileTypes.searchable) != nil &&
            fileTypesDict[FileTypes.searchable]!.contains(FileUtil.getExtension(fileName))
    }

    open func isTextFile(_ fileName: String) -> Bool {
        return fileTypesDict.index(forKey: FileTypes.text) != nil &&
            fileTypesDict[FileTypes.text]!.contains(FileUtil.getExtension(fileName))
    }

    open func isUnknownFile(_ fileName: String) -> Bool {
        return (fileTypesDict.index(forKey: FileTypes.unknown) != nil &&
            fileTypesDict[FileTypes.unknown]!.contains(FileUtil.getExtension(fileName)))
            || !isSearchableFile(fileName)
    }
    
    open func isXmlFile(_ fileName: String) -> Bool {
        return fileTypesDict.index(forKey: FileTypes.xml) != nil &&
            fileTypesDict[FileTypes.xml]!.contains(FileUtil.getExtension(fileName))
    }
}
