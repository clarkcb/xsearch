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
    println(s)
}

func logError(s: String) {
    logMsg("ERROR: \(s)")
}

func exitWithError(s: String) {
    logError(s)
    exit(1 as Int32)
}

// from http://ijoshsmith.com/2014/06/18/create-a-swift-dictionary-from-an-array/
func toDictionary<E, K, V>(
    array: [E],
    transformer: (element: E) -> (key: K, value: V)?)
    -> Dictionary<K, V>
{
    return array.reduce([:]) {
        (var dict, e) in
        if let (key, value) = transformer(element: e)
        {
            dict[key] = value
        }
        return dict
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
        count++
    }
    str += "]"
    return str
}

func arrayToString(arr: [Regex]) -> String {
    var str: String = "["
    var count: Int = 0
    for r in arr {
        if count > 0 {
            str += ", "
        }
        str += "\"\(r.pattern)\""
        count++
    }
    str += "]"
    return str
}

func setToString(set:Set<String>) -> String {
    return arrayToString(Array(sorted(set)))
}

func dropRight<T>(seq: [T], num: Int) -> [T] {
    let newCount = seq.count - num
    var newSeq: [T] = []
    for var i=0; i < newCount; ++i {
        newSeq.append(seq[i])
    }
    return newSeq
}

func take<T>(seq: [T], num: Int) -> [T] {
    var newSeq: [T] = []
    var taken = 0
    while taken < num && taken < seq.count {
        newSeq.append(seq[taken])
        ++taken
    }
    return newSeq
}

func takeRight<T>(seq: [T], num: Int) -> [T] {
    var right: [T] = []
    var sub = 1
    while sub <= num && sub <= seq.count {
        right.append(seq[seq.count - sub])
        ++sub
    }
    return right.reverse()
}

// for printing the borders in multiline search results
extension String {
    func repeat(n: Int) -> String {
        var result = self
        for _ in 1 ..< n {
            result.extend(self)
        }
        return result
    }
}
