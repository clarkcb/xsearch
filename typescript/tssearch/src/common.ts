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
    const hash: {[key:string]: boolean} = {};
    arr.forEach(a => hash[a] = true);
    return hash;
}

export function setFromArray(arr: string[]): string[] {
    const hash: {[key:string]: boolean} = boolHashFromArray(arr);
    const set: string[] = [];
    for (const k in hash) {
        if (hash.hasOwnProperty(k)) {
            set.push(k);
        }
    }
    return set;
}

export const COLORS = {
    GREY: '\u001b[30m',
    RED: '\u001b[31m',
    GREEN: '\u001b[32m',
    YELLOW: '\u001b[33m',
    BLUE: '\u001b[34m',
    RESET: '\u001b[0m'
}
