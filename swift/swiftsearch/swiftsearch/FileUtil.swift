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

    static func expandPath(_ filePath: String) -> String {
        if filePath.hasPrefix("~") {
           let homePath = ProcessInfo.init().environment["HOME"]
            var expanded: String? = homePath
            if expanded != nil {
                if filePath.lengthOfBytes(using: String.Encoding.utf8) > 1 {
                    let index = filePath.index(filePath.startIndex, offsetBy: 1)
                    expanded!.append(String(filePath[index...]))
                }
                return expanded!
            }
        }
        return filePath
    }

    static func getExtension(_ fileName: String) -> String {
        let ext = NSURL(fileURLWithPath: fileName).pathExtension?.uppercased()
        if ext == "Z" {
            return ext!
        }
        return ext!.lowercased()
    }

    static func hasExtension(_ fileName: String, ext: String) -> Bool {
        return getExtension(fileName) == ext.lowercased()
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
        let pathElems = filePath.split {$0 == "/"}.map { String($0) }
        return pathElems.filter({self.isHiddenFile($0)}).count > 0
    }

    static func isHiddenFile(_ fileName: String) -> Bool {
        return fileName.hasPrefix(".") && !isDotDir(fileName)
    }
    
    static func splitPath(_ filePath: String) -> (String, String) {
        let pathElems = filePath.split {$0 == "/"}.map { String($0) }
        if pathElems.count > 1 {
            let path = pathElems.prefix(pathElems.count-1).joined(separator: separator)
            return (path, pathElems[pathElems.count-1])
        } else {
            return ("", filePath)
        }
    }
    
    static func joinPath(_ path: String, childPath: String) -> String {
        if path.hasSuffix(separator) {
            return "\(path)\(childPath)"
        } else {
            return "\(path)/\(childPath)"
        }
    }
}
