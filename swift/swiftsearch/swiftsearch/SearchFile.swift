//
//  SearchFile.swift
//  swiftsearch
//
//  Created by Cary Clark on 6/2/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

class SearchFile {
    let containerSeparator = "!"
    let containers: [String]
    let filePath: String
    let fileType: FileType

    init(filePath: String, fileType: FileType) {
        self.filePath = filePath
        self.fileType = fileType
        containers = []
    }

    func description() -> String {
        var str = "\(containers.isEmpty ? "" : containers.joined(separator: containerSeparator) + containerSeparator)"
        str += filePath
        return str
    }
}
