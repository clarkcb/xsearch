/*
 * common.js
 *
 * Some common functions, etc.
 */

'use strict'

// add a startsWith method to String type
if (typeof String.prototype.startsWith !== 'function') {
    String.prototype.startsWith = (str) => {
        return this.slice(0, str.length) === str;
    };
}

const log = (message) => console.log(message);
exports.log = log;

const boolHashFromArray = (arr) => {
    const hash = {};
    arr.forEach(a => hash[a] = true);
    return hash;
};
exports.boolHashFromArray = boolHashFromArray;

const setFromArray = (arr) => {
    let hash = boolHashFromArray(arr);
    let set = [];
    for (let k in hash) {
        if (hash.hasOwnProperty(k)) {
            set.push(k);
        }
    }
    return set;
};
exports.setFromArray = setFromArray;

const COLORS = {
    GREY: '\u001b[30m',
    RED: '\u001b[31m',
    GREEN: '\u001b[32m',
    YELLOW: '\u001b[33m',
    BLUE: '\u001b[34m',
    RESET: '\u001b[0m'
}
exports.COLORS = COLORS;
