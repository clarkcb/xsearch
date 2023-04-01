//
//  Config.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public struct Config {
    public let xsearchPath: String
    public let sharedPath: String
    public let fileTypesPath: String
    public let searchOptionsPath: String

    public init() {
        if let xsearchEnvPath = ProcessInfo.processInfo.environment["XSEARCH_PATH"] {
            self.xsearchPath = xsearchEnvPath
        } else {
            self.xsearchPath = "\(NSHomeDirectory())/src/xsearch"
        }
        self.sharedPath = "\(xsearchPath)/shared"
        self.fileTypesPath = "\(sharedPath)/filetypes.json"
        self.searchOptionsPath = "\(sharedPath)/searchoptions.json"
    }
}
