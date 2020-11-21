//
//  FileUtil.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/18/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

open class FileUtil {
    fileprivate static let dotDirs = Set<String>([".", "..", "./", "../"])
    fileprivate static let separator = "/"

    public static func expandPath(_ filePath: String) -> String {
        if filePath.hasPrefix("~") {
            let homePath = ProcessInfo().environment["HOME"]
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

    public static func getExtension(_ fileName: String) -> String {
        let ext = NSURL(fileURLWithPath: fileName).pathExtension?.uppercased()
        if ext == "Z" {
            return ext!
        }
        return ext!.lowercased()
    }

    public static func hasExtension(_ fileName: String, ext: String) -> Bool {
        getExtension(fileName) == ext.lowercased()
    }

    fileprivate static func getFileManager() -> FileManager {
        FileManager.default
    }

    // gets files only directly under given path
    public static func contentsForPath(_ filePath: String) -> [String] {
        do {
            return try getFileManager().contentsOfDirectory(atPath: filePath)
        } catch {
            return []
        }
    }

    // gets files recursively under given path
    public static func enumeratorForPath(_ filePath: String) -> FileManager.DirectoryEnumerator? {
        getFileManager().enumerator(atPath: filePath)
    }

    public static func exists(_ filePath: String) -> Bool {
        getFileManager().fileExists(atPath: filePath)
    }

    public static func isDirectory(_ filePath: String) -> Bool {
        var isDir: ObjCBool = false
        if getFileManager().fileExists(atPath: filePath, isDirectory: &isDir) {
            return isDir.boolValue
        } else {
            logMsg("ERROR: filepath not found: \(filePath)")
            return false
        }
    }

    public static func isReadableFile(_ filePath: String) -> Bool {
        getFileManager().isReadableFile(atPath: filePath)
    }

    public static func isDotDir(_ filePath: String) -> Bool {
        dotDirs.contains(filePath)
    }

    public static func isHidden(_ filePath: String) -> Bool {
        let pathElems = filePath.split { $0 == "/" }.map { String($0) }
        return pathElems.filter { self.isHiddenFile($0) }.count > 0
    }

    public static func isHiddenFile(_ fileName: String) -> Bool {
        fileName.hasPrefix(".") && !isDotDir(fileName)
    }

    public static func splitPath(_ filePath: String) -> (String, String) {
        let pathElems = filePath.split { $0 == "/" }.map { String($0) }
        if pathElems.count > 1 {
            let path = pathElems.prefix(pathElems.count - 1).joined(separator: separator)
            return (path, pathElems[pathElems.count - 1])
        } else {
            return ("", filePath)
        }
    }

    public static func joinPath(_ path: String, childPath: String) -> String {
        if path.hasSuffix(separator) {
            return "\(path)\(childPath)"
        } else {
            return "\(path)/\(childPath)"
        }
    }
}
