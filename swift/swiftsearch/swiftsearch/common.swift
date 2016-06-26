//
//  common.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/20/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

let whitespace = NSCharacterSet(charactersInString: " \t\r\n")

func logMsg(s: String) {
    print(s)
}

func logError(s: String) {
    logMsg("ERROR: \(s)")
}

func setError(error: NSErrorPointer, msg: String) {
    error.memory = NSError(domain: msg, code: 1, userInfo: [:])
}

// from http://ijoshsmith.com/2014/06/18/create-a-swift-dictionary-from-an-array/
func toDictionary<E, K, V>(
    array: [E],
    transformer: (element: E) -> (key: K, value: V)?)
    -> Dictionary<K, V>
{
    return array.reduce([:]) {
        (dict, e) in
        var d = dict
        if let (key, value) = transformer(element: e)
        {
            d[key] = value
        }
        return d
    }
}

func arrayToString(arr: [String]) -> String {
    var str: String = "["
    var count: Int = 0
    for s in arr {
        if count > 0 {
            str += ", "
        }
        str += "\"\(s)\""
        count += 1
    }
    str += "]"
    return str
}

func arrayToString(arr: [Regex]) -> String {
    return arrayToString(arr.map({$0.pattern}))
}

func setToString(set:Set<String>) -> String {
    return arrayToString(Array(set.sort()))
}

func take<T>(seq: [T], num: Int) -> [T] {
    var newSeq: [T] = []
    var taken = 0
    while taken < num && taken < seq.count {
        newSeq.append(seq[taken])
        taken += 1
    }
    return newSeq
}

func takeRight<T>(seq: [T], num: Int) -> [T] {
    var right: [T] = []
    var sub = 1
    while sub <= num && sub <= seq.count {
        right.append(seq[seq.count - sub])
        sub += 1
    }
    return Array(right.reverse())
}

// for printing the borders in multiline search results
extension String {
    func `repeat`(n: Int) -> String {
        var result = self
        for _ in 1 ..< n {
            result += self
        }
        return result
    }
}
