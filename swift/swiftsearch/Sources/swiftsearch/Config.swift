//
//  Config.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/12/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public class Config {
    // static let xsearchPath = NSString(string: "~/src/xsearch").stringByExpandingTildeInPath
    public static let xsearchPath = "\(NSHomeDirectory())/src/xsearch"
    public static let sharedPath = "\(xsearchPath)/shared"
    // public static let fileTypesPath = "\(sharedPath)/filetypes.xml"
    public static let fileTypesPath = "\(sharedPath)/filetypes.json"
    // public static let searchOptionsPath = "\(sharedPath)/searchoptions.xml"
    public static let searchOptionsPath = "\(sharedPath)/searchoptions.json"
}
