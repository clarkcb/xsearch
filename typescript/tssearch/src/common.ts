/*
 * common.ts
 *
 * Some common functions, etc.
 */

"use strict";

import {ConsoleColor} from 'tsfind';

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

export function logError(message: string, colorize: boolean = true): void {
    if (colorize) {
        console.error(`${ConsoleColor.BOLD_RED}${message}${ConsoleColor.RESET}`);
    } else {
        console.error(message);
    }
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
