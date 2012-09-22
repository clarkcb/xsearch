/*
 * nodesearch.js
 *
 * file search utility written in node.js
 */

var Searcher = require('./searcher.js').Searcher;
var SearchOptions = require('./searchoptions.js').SearchOptions;
var SearchSettings = require('./searchsettings.js').SearchSettings;

function searchMain() {
    var searchOptions = new SearchOptions();

    if (process.argv.length < 3) {
        throw new Error("Missing arguments");
    }
    var args = process.argv.slice(2);
    //console.log('args: ', args);

    var settings = searchOptions.searchSettingsFromArgs(args);
    if (!settings.startPath) {
        throw new Error("Missing startpath");
    }
    if (!settings.searchPatterns.length) {
        throw new Error("Search pattern not defined");
    }
    if (settings.debug) {
        console.log("settings: " + settings.toString());
    }
    
    var searcher = new Searcher(settings);
    searcher.search();
    
    console.log("Matches: " + searcher.results.length);
    
    if (settings.listfiles) {
        var files = searcher.getFilesWithMatches();
        console.log("\nMatching files:");
        for (var f in files) {
            console.log(files[f]);
        }
    }
}

// node.js equivalent of python's if __name__ == '__main__'
if (!module.parent) {
    searchMain();
}
