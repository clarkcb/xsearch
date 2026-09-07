//
//  Config.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

import swiftfind

public struct SearchConfig {
    public let findConfig: FindConfig
    public let xsearchPath: String
    public let sharedPath: String
    public let searchOptionsPath: String
    public let defaultSearchSettingsPath: String

    public init() {
        findConfig = FindConfig()
        var xSearchConfigDir: String
        if let xSearchEnvConfigDir = ProcessInfo.processInfo.environment["XSEARCH_CONFIG_DIR"] {
            xSearchConfigDir = xSearchEnvConfigDir
        } else {
            xSearchConfigDir = "\(NSHomeDirectory())/.config/xsearch"
        }
        if let xsearchEnvPath = ProcessInfo.processInfo.environment["XSEARCH_PATH"] {
            xsearchPath = xsearchEnvPath
        } else {
            xsearchPath = "\(NSHomeDirectory())/src/xsearch"
        }
        sharedPath = "\(xsearchPath)/shared"
        searchOptionsPath = "\(sharedPath)/searchoptions.json"
        defaultSearchSettingsPath = "\(xSearchConfigDir)/settings.json"
    }
}
