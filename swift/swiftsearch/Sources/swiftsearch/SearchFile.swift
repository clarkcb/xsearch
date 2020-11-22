//
//  SearchFile.swift
//  swiftsearch
//
//  Created by Cary Clark on 6/2/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public struct SearchFile {
    public let containerSeparator = "!"
    public let containers: [String]
    public let filePath: String
    public let fileType: FileType

    public init(filePath: String, fileType: FileType) {
        self.filePath = filePath
        self.fileType = fileType
        containers = []
    }

    public var description: String {
        "\(containers.isEmpty ? "" : containers.joined(separator: containerSeparator) + containerSeparator)" +
        filePath
    }
}
