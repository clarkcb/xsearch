/*
 * common.js
 *
 * Some common functions, etc.
 */

// add a startsWith method to String type
if (typeof String.prototype.startsWith != 'function') {
    String.prototype.startsWith = function (str) {
        return this.slice(0, str.length) == str;
    };
}

// add a format method to String type (use {0} style placeholders)
// from http://stackoverflow.com/questions/610406/javascript-equivalent-to-printf-string-format/4673436#4673436
if (!String.prototype.format) {
  String.prototype.format = function() {
    var args = arguments;
    return this.replace(/{(\d+)}/g, function(match, number) { 
      return typeof args[number] != 'undefined' ? args[number] : match;
    });
  };
}

var log = function (message) {
    console.log(message);
};
exports.log = log;

var boolHashFromArray = function (arr) {
    var hash = {};
    for (var a in arr) {
        hash[arr[a]] = true;
    }
    return hash;
};
exports.boolHashFromArray = boolHashFromArray;

var setFromArray = function (arr) {
    var hash = boolHashFromArray(arr);
    var set = [];
    for (var h in hash) {
        set.push(h);
    }
    return set;
};
exports.setFromArray = setFromArray;
