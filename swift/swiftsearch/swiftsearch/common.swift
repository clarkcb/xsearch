//
//  common.swift
//  swiftsearch
//
//  Created by Cary Clark on 5/20/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

let whitespace = CharacterSet(charactersIn: " \t\r\n")

func logMsg(_ s: String) {
    print(s)
}

func logError(_ s: String) {
    logMsg("ERROR: \(s)")
}

func setError(_ error: NSErrorPointer, msg: String) {
    error?.pointee = NSError(domain: msg, code: 1, userInfo: [:])
}

// from http://ijoshsmith.com/2014/06/18/create-a-swift-dictionary-from-an-array/
func toDictionary<E, K, V>(
    _ array: [E],
    transformer: (_ element: E) -> (key: K, value: V)?)
    -> Dictionary<K, V>
{
    return array.reduce([:]) {
        (dict, e) in
        var d = dict
        if let (key, value) = transformer(e)
        {
            d[key] = value
        }
        return d
    }
}

func arrayToString(_ arr: [String]) -> String {
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

func arrayToString(_ arr: [Regex]) -> String {
    return arrayToString(arr.map({$0.pattern}))
}

func setToString(_ set:Set<String>) -> String {
    return arrayToString(Array(set.sorted()))
}

func take<T>(_ seq: [T], num: Int) -> [T] {
    var newSeq: [T] = []
    var taken = 0
    while taken < num && taken < seq.count {
        newSeq.append(seq[taken])
        taken += 1
    }
    return newSeq
}

func takeRight<T>(_ seq: [T], num: Int) -> [T] {
    var right: [T] = []
    var sub = 1
    while sub <= num && sub <= seq.count {
        right.append(seq[seq.count - sub])
        sub += 1
    }
    return Array(right.reversed())
}

// for printing the borders in multiline search results
extension String {
    func `repeat`(_ n: Int) -> String {
        var result = self
        for _ in 1 ..< n {
            result += self
        }
        return result
    }
}
