/// <reference path="../typings/node/node.d.ts"/>
/*
 * common.ts
 *
 * Some common functions, etc.
 */

"use strict";

interface String {
    startsWith(str: string): boolean;
}

// add a startsWith method to String type
if (typeof String.prototype.startsWith !== 'function') {
    String.prototype.startsWith = function (str: string): boolean {
        return this.slice(0, str.length) === str;
    };
}

var log = function (message: string): void {
    console.log(message);
};
exports.log = log;

var boolHashFromArray = function (arr: string[]): {[key:string]: boolean} {
    let hash: {[key:string]: boolean} = {};
    arr.forEach(a => hash[a] = true);
    return hash;
};
exports.boolHashFromArray = boolHashFromArray;

var setFromArray = function (arr: string[]): string[] {
    let hash: {[key:string]: boolean} = boolHashFromArray(arr);
    let set: string[] = [];
    for (let k in hash) {
        if (hash.hasOwnProperty(k)) {
            set.push(k);
        }
    }
    return set;
};
exports.setFromArray = setFromArray;
