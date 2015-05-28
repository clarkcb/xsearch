//
//  FileUtil.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

class FileUtil {
    private static let dotDirs = Set<String>([".", ".."])
    private static let separator = "/"

    static func getExtension(fileName: String) -> String {
        let ext = fileName.pathExtension
        if ext == "Z" {
            return ext
        }
        return ext.lowercaseString
    }

    static func hasExtension(fileName: String, ext: String) -> Bool {
        return (getExtension(fileName) == ext)
    }

    private static func getFileManager() -> NSFileManager {
        return NSFileManager.defaultManager()
    }

    static func contentsForPath(filePath: String) -> [String] {
        return getFileManager().contentsOfDirectoryAtPath(filePath, error: nil) as! [String]
    }

    static func enumeratorForPath(filePath: String) -> NSDirectoryEnumerator? {
        return getFileManager().enumeratorAtPath(filePath)
    }

    static func exists(filePath: String) -> Bool {
        return getFileManager().fileExistsAtPath(filePath)
    }

    static func isDirectory(filePath: String) -> Bool {
        var isDir: ObjCBool = false
        if getFileManager().fileExistsAtPath(filePath, isDirectory:&isDir) {
            return isDir.boolValue
        } else {
            logMsg("ERROR: filepath not found: \(filePath)")
            return false
        }
    }

    static func isReadableFile(filePath: String) -> Bool {
        return getFileManager().isReadableFileAtPath(filePath)
    }

    static func isDotDir(filePath: String) -> Bool {
        return dotDirs.contains(filePath)
    }

    static func isHidden(filePath: String) -> Bool {
        let pathElems = splitPath(filePath)
        let hidden = pathElems.filter({self.isHiddenFile($0)})
        return hidden.count > 0
    }

    static func isHiddenFile(fileName: String) -> Bool {
        return fileName.hasPrefix(".") && !isDotDir(fileName)
    }

    static func splitPath(filePath: String) -> [String] {
        return split(filePath) {$0 == "/"}
    }
}
