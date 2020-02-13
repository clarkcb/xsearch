/*
 * common.ts
 *
 * Some common functions, etc.
 */

"use strict";

export interface String {
    startsWith(str: string): boolean;
}

// add a startsWith method to String type
if (typeof String.prototype.startsWith !== 'function') {
    String.prototype.startsWith = function (str: string): boolean {
        return this.slice(0, str.length) === str;
    };
}

export function log(message: string): void {
    console.log(message);
}

export function boolHashFromArray(arr: string[]): {[key:string]: boolean} {
    let hash: {[key:string]: boolean} = {};
    arr.forEach(a => hash[a] = true);
    return hash;
}

export function setFromArray(arr: string[]): string[] {
    let hash: {[key:string]: boolean} = boolHashFromArray(arr);
    let set: string[] = [];
    for (let k in hash) {
        if (hash.hasOwnProperty(k)) {
            set.push(k);
        }
    }
    return set;
}
