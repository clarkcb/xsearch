/*
 * nodesearch.js
 *
 * file search utility written in node.js
 */

function SearchSettings() {
    var that = this;
    this.DEFAULT_OUT_DIRPATTERNS = [
        new RegExp("\\bCVS$"),
        new RegExp("\\.git$"),
        new RegExp("\\.svn$")]
    this.DEFAULT_OUT_FILEPATTERNS = [new RegExp("^\\.DS_Store$")]
    this.startPath = "";
    this.inExtensions = [];
    this.outExtensions = [];
    this.inDirPatterns = [];
    this.outDirPatterns = this.DEFAULT_OUT_DIRPATTERNS;
    this.inFilePatterns = [];
    this.outFilePatterns = this.DEFAULT_OUT_FILEPATTERNS;
    this.inArchiveFilePatterns = [];
    this.outArchiveFilePatterns = [];
    this.inLinesAfterPatterns = [];
    this.outLinesAfterPatterns = [];
    this.inLinesBeforePatterns = [];
    this.outLinesBeforePatterns = [];
    this.linesAfterToPatterns = [];
    this.linesAfterUntilPatterns = [];
    this.searchPatterns = [];
    this.archivesOnly = false;
    this.debug = false;
    this.doTiming = false;
    this.firstMatch = false;
    this.linesAfter = 0;
    this.linesBefore = 0;
    this.listDirs = false;
    this.listFiles = false;
    this.listLines = false;
    this.multilineSearch = false;
    this.printResults = false;
    this.printUsage = false;
    this.printVersion = false;
    this.searchArchives = false;
    this.uniqueLines = false;
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
    this.addInDirPattern = function (pattern) {
        addPattern(pattern, that.inDirPatterns);
    };
    this.addOutDirPattern = function (pattern) {
        addPattern(pattern, that.outDirPatterns);
    };
    this.addInFilePattern = function (pattern) {
        addPattern(pattern, that.inFilePatterns);
    };
    this.addOutFilePattern = function (pattern) {
        addPattern(pattern, that.outFilePatterns);
    };
    this.addSearchPattern = function (pattern) {
        addPattern(pattern, that.searchPatterns);
    };
    this.addInArchiveFilePattern = function (pattern) {
        addPattern(pattern, that.inArchiveFilePatterns);
    };
    this.addOutArchiveFilePattern = function (pattern) {
        addPattern(pattern, that.outArchiveFilePatterns);
    };
    this.addInLinesAfterPattern = function (pattern) {
        addPattern(pattern, that.inLinesAfterPatterns);
    };
    this.addOutLinesAfterPattern = function (pattern) {
        addPattern(pattern, that.outLinesAfterPatterns);
    };
    this.addInLinesBeforePattern = function (pattern) {
        addPattern(pattern, that.inLinesBeforePatterns);
    };
    this.addOutLinesBeforePattern = function (pattern) {
        addPattern(pattern, that.outLinesBeforePatterns);
    };

    this.addLinesAfterToPattern = function (pattern) {
        addPattern(pattern, that.linesAfterToPatterns);
    };
    this.addLinesAfterUntilPattern = function (pattern) {
        addPattern(pattern, that.linesAfterUntilPatterns);
    };

    this.toString = function () {
        var s = 'SearchSettings(startPath="' + that.startPath + '"';
        if (that.inExtensions.length) {
            s = s + ', inExtensions=["' + that.inExtensions.join('","') + '"]';
        }
        if (that.outExtensions.length) {
            s = s + ', outExtensions=["' + that.outExtensions.join('","') + '"]';
        }
        if (that.inDirPatterns.length) {
            s = s + ', inDirPatterns=["' + that.inDirPatterns.join('","') + '"]';
        }
        if (that.outDirPatterns.length) {
            s = s + ', outDirPatterns=["' + that.outDirPatterns.join('","') + '"]';
        }
        if (that.inFilePatterns.length) {
            s = s + ', inFilePatterns=["' + that.inFilePatterns.join('","') + '"]';
        }
        if (that.outFilePatterns.length) {
            s = s + ', outFilePatterns=["' + that.outFilePatterns.join('","') + '"]';
        }
        if (that.searchPatterns.length) {
            s = s + ', searchPatterns=["' + that.searchPatterns.join('","') + '"]';
        }
        s = s + ', doTiming=' + that.doTiming + ', verbose=' + that.verbose + ')';
        return s;
    };
}

exports.SearchSettings = SearchSettings;