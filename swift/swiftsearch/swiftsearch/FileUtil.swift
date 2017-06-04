//
//  FileUtil.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

class FileUtil {
    fileprivate static let dotDirs = Set<String>([".", ".."])
    fileprivate static let separator = "/"

    static func getExtension(_ fileName: String) -> String {
        let ext = NSURL(fileURLWithPath: fileName).pathExtension?.uppercased()
        if ext == "Z" {
            return ext!
        }
        return ext!.lowercased()
    }

    static func hasExtension(_ fileName: String, ext: String) -> Bool {
        return getExtension(fileName) == ext
    }

    fileprivate static func getFileManager() -> FileManager {
        return FileManager.default
    }

    // gets files only directly under given path
    static func contentsForPath(_ filePath: String) -> [String] {
        return (try! getFileManager().contentsOfDirectory(atPath: filePath)) 
    }

    // gets files recursively under given path
    static func enumeratorForPath(_ filePath: String) -> FileManager.DirectoryEnumerator? {
        return getFileManager().enumerator(atPath: filePath)
    }

    static func exists(_ filePath: String) -> Bool {
        return getFileManager().fileExists(atPath: filePath)
    }

    static func isDirectory(_ filePath: String) -> Bool {
        var isDir: ObjCBool = false
        if getFileManager().fileExists(atPath: filePath, isDirectory:&isDir) {
            return isDir.boolValue
        } else {
            logMsg("ERROR: filepath not found: \(filePath)")
            return false
        }
    }

    static func isReadableFile(_ filePath: String) -> Bool {
        return getFileManager().isReadableFile(atPath: filePath)
    }

    static func isDotDir(_ filePath: String) -> Bool {
        return dotDirs.contains(filePath)
    }

    static func isHidden(_ filePath: String) -> Bool {
        let pathElems = splitPath(filePath)
        return pathElems.filter({self.isHiddenFile($0)}).count > 0
    }

    static func isHiddenFile(_ fileName: String) -> Bool {
        return fileName.hasPrefix(".") && !isDotDir(fileName)
    }
    
    static func splitPath(_ filePath: String) -> [String] {
        return filePath.characters.split {$0 == "/"}.map { String($0) }
    }
    
    static func joinPath(_ path: String, childPath: String) -> String {
        if path[path.index(before: path.endIndex)] == "/" {
            return "\(path)\(childPath)"
        } else {
            return "\(path)/\(childPath)"
        }
    }
}
