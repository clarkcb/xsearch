/*
 * nodesearch.js
 *
 * file search utility written in node.js
 */

var searcher = require('./searcher.js');
var Searcher = searcher.Searcher;
var searchsettings = require('./searchsettings.js');
var SearchSettings = searchsettings.SearchSettings;

function searchSettingsFromArgs(args) {
    var settings = new SearchSettings();
    var argMap = {
        'd': settings.addInDirnamePattern,
        'D': settings.addOutDirnamePattern,
        'f': settings.addInFilenamePattern,
        'F': settings.addOutFilenamePattern,
        's': settings.addSearchPattern,
        'x': settings.addInExtension,
        'X': settings.addOutExtension
    };
    var flagMap = {
        'debug': function () { settings.debug = true; },
        'listfiles': function () { settings.listfiles = true; },
        't': function () { settings.doTiming = true; },
        'v': function () { settings.verbose = true; },
        '1': function () { settings.firstMatch = true; }
    };
    while(args) {
        var arg = args.shift();
        if (!arg) {
            break;
        }
        if (arg.charAt(0) === '-') {
            while (arg && arg.charAt(0) === '-') {
                arg = arg.substring(1);
            }
            if (argMap[arg]) {
                if (args) {
                    argMap[arg](args.shift());
                } else {
                    throw new Error("Missing argument for option "+arg);
                }
            } else if (flagMap[arg]) {
                flagMap[arg]();
            } else {
                throw new Error("Unknown option: "+arg);
            }
        } else {
            settings.startPath = arg;
        }
    }
    return settings;
}

function searchMain() {
    if (process.argv.length < 3) {
        throw new Error("Missing arguments");
    }
    var args = process.argv.slice(2);
    //console.log('args: ', args);
    settings = searchSettingsFromArgs(args);
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
