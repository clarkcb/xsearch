/*
 * common.js
 *
 * Some common functions, etc.
 */

"use strict";

// add a startsWith method to String type
if (typeof String.prototype.startsWith !== 'function') {
    String.prototype.startsWith = (str) => {
        return this.slice(0, str.length) === str;
    };
}

const log = (message) => console.log(message);
exports.log = log;

const boolHashFromArray = (arr) => {
    let hash = {};
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
