//
//  SearchError.swift
//  
//
//  Created by Cary Clark on 12/29/21.
//

import Foundation

public class SearchError: Error {
    public let msg: String

    public init(msg: String) {
        self.msg = msg
    }
}
