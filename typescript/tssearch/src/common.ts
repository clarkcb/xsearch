/// <reference path="../typings/node/node.d.ts"/>
/*
 * common.ts
 *
 * Some common functions, etc.
 */

"use strict";

interface String {
    format(): string;
    startsWith(str: string): boolean;
}

// add a format method to String type (use {0} style placeholders)
// from http://stackoverflow.com/questions/610406/javascript-equivalent-to-printf-string-format/4673436#4673436
if (typeof String.prototype.format !== 'function') {
    String.prototype.format = function(): string {
        var args = arguments;
        return this.replace(/{(\d+)}/g, function (match: string, n: number) {
            return typeof args[n] !== 'undefined' ? args[n] : match;
        });
    };
}

// add a startsWith method to String type
if (typeof String.prototype.startsWith !== 'function') {
    String.prototype.startsWith = function (str: string): boolean {
        return this.slice(0, str.length) == str;
    };
}

var log = function (message: string): void {
    console.log(message);
};
exports.log = log;

var logArray = function (message: string, arr: any[]): void {
    log(message);
    for (var i: number = 0; i < arr.length; i++) {
        log(arr[i]);
    }
};
exports.logArray = logArray;

var boolHashFromArray = function (arr: string[]): {[key:string]: boolean} {
    var hash: {[key:string]: boolean} = {};
    for (var i: number = 0; i < arr.length; i++) {
        hash[arr[i]] = true;
    }
    return hash;
};
exports.boolHashFromArray = boolHashFromArray;

var setFromArray = function (arr: string[]): string[] {
    var hash: {[key:string]: boolean} = boolHashFromArray(arr);
    var set: string[] = [];
    for (var k in hash) {
        if (hash.hasOwnProperty(k)) {
            set.push(k);
        }
    }
    return set;
};
exports.setFromArray = setFromArray;
