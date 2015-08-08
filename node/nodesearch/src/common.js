/*
 * common.js
 *
 * Some common functions, etc.
 */

// add a startsWith method to String type
if (typeof String.prototype.startsWith !== 'function') {
    String.prototype.startsWith = function (str) {
        return this.slice(0, str.length) == str;
    };
}

// add a format method to String type (use {0} style placeholders)
// from http://stackoverflow.com/questions/610406/javascript-equivalent-to-printf-string-format/4673436#4673436
if (!String.prototype.format) {
  String.prototype.format = function() {
    var args = arguments;
    return this.replace(/{(\d+)}/g, function(match, n) { 
      return typeof args[n] !== 'undefined' ? args[n] : match;
    });
  };
}

var log = function (message) {
    console.log(message);
};
exports.log = log;

var logArray = function (message, arr) {
    log(message);
    for (var i = 0; i < arr.length; i++) {
        log(arr[i]);
    }
};
exports.logArray = logArray;

var boolHashFromArray = function (arr) {
    var hash = {};
    for (var i = 0; i < arr.length; i++) {
        hash[arr[i]] = true;
    }
    return hash;
};
exports.boolHashFromArray = boolHashFromArray;

var setFromArray = function (arr) {
    var hash = boolHashFromArray(arr);
    var set = [];
    for (var k in hash) {
        if (hash.hasOwnProperty(k)) {
            set.push(k);
        }
    }
    return set;
};
exports.setFromArray = setFromArray;
