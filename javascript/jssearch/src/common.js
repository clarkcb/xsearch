/*
 * common.js
 *
 * Some common functions, etc.
 */

"use strict";

// add a startsWith method to String type
if (typeof String.prototype.startsWith !== 'function') {
    String.prototype.startsWith = function (str) {
        return this.slice(0, str.length) === str;
    };
}

var log = (message) => console.log(message);
exports.log = log;

var boolHashFromArray = function (arr) {
    let hash = {};
    arr.forEach(a => hash[a] = true);
    return hash;
};
exports.boolHashFromArray = boolHashFromArray;

var setFromArray = function (arr) {
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
