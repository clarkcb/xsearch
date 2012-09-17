/*
 * nodesearch.js
 *
 * file search utility written in node.js
 */

function SearchSettings() {
    var that = this;
    this.startPath = "";
    this.inExtensions = [];
    this.outExtensions = [];
    this.inDirnamePatterns = [];
    this.outDirnamePatterns = [];
    this.inFilenamePatterns = [];
    this.outFilenamePatterns = [];
    this.searchPatterns = [];
    this.debug = false;
    this.doTiming = false;
    this.firstMatch = false;
    this.listFiles = false;
    this.verbose = false;
    var addExtension = function (ext, arr) {
        arr.push(ext);
    };
    this.addInExtension = function (ext) {
        addExtension(ext, that.inExtensions);
    };
    this.addOutExtension = function (ext) {
        addExtension(ext, that.outExtensions);
    };
    var addPattern = function (pattern, arr) {
        arr.push(new RegExp(pattern));
    };
    this.addInDirnamePattern = function (pattern) {
        addPattern(pattern, that.inDirnamePatterns);
    };
    this.addOutDirnamePattern = function (pattern) {
        addPattern(pattern, that.outDirnamePatterns);
    };
    this.addInFilenamePattern = function (pattern) {
        addPattern(pattern, that.inFilenamePatterns);
    };
    this.addOutFilenamePattern = function (pattern) {
        addPattern(pattern, that.outFilenamePatterns);
    };
    this.addSearchPattern = function (pattern) {
        addPattern(pattern, that.searchPatterns);
    };
    this.toString = function () {
        var s = 'SearchSettings(startPath="' + that.startPath + '"';
        if (that.inExtensions.length) {
            s = s + ', inExtensions=["' + that.inExtensions.join('","') + '"]';
        }
        if (that.outExtensions.length) {
            s = s + ', outExtensions=["' + that.outExtensions.join('","') + '"]';
        }
        if (that.inFilenamePatterns.length) {
            s = s + ', inFilenamePatterns=["' + that.inFilenamePatterns.join('","') + '"]';
        }
        if (that.outFilenamePatterns.length) {
            s = s + ', outFilenamePatterns=["' + that.outFilenamePatterns.join('","') + '"]';
        }
        if (that.searchPatterns.length) {
            s = s + ', searchPatterns=["' + that.searchPatterns.join('","') + '"]';
        }
        s = s + ', doTiming=' + that.doTiming + ', verbose=' + that.verbose + ')';
        return s;
    };
};

exports.SearchSettings = SearchSettings;