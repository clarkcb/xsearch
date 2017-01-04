//
//  SearchFile.swift
//  swiftsearch
//
//  Created by Cary Clark on 6/2/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

class SearchFile: CustomStringConvertible {
    let containerSeparator = "!"
    let containers: [String] = []
    let path: String
    let fileName: String
    let fileType: String

    init(path: String, fileName: String, fileType: String) {
        self.path = path
        self.fileName = fileName
        self.fileType = fileType
    }

    var description: String {
        var s = ""
        let url = URL(fileURLWithPath: path)
        if !containers.isEmpty {
            s += containers.joined(separator: containerSeparator) + containerSeparator
        }
        s += url.appendingPathComponent(fileName).absoluteString
        return s
    }
}
