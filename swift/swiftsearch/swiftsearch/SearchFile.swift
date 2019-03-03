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
    let containers: [String]
    let filePath: String
    let fileType: FileType

    init(filePath: String, fileType: FileType) {
        self.filePath = filePath
        self.fileType = fileType
        self.containers = []
    }

    var description: String {
        var s = ""
        if !containers.isEmpty {
            s += containers.joined(separator: containerSeparator) + containerSeparator
        }
        s += filePath
        return s
    }
}
