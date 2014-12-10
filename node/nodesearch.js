/*
 * nodesearch.js
 *
 * file search utility written in node.js
 */

var Searcher = require('./searcher.js').Searcher;
var SearchOptions = require('./searchoptions.js').SearchOptions;

function searchMain() {
    var searchOptions = new SearchOptions();

    var args = process.argv.slice(2);
    //console.log('args: ', args);

    var settings;
    try {
        settings = searchOptions.searchSettingsFromArgs(args);
    } catch (err) {
        console.log('\nException: ' + err + '\n');
        searchOptions.usageWithCode(1);
    }

    if (settings.printUsage)
        searchOptions.usage();

    if (settings.printVersion) {
        console.log('Version: 0.1');
        process.exit(0);
    }

    if (settings.debug)
        console.log("settings: " + settings.toString());

     try {
        var searcher = new Searcher(settings);
        searcher.search();

        if (settings.printResults) {
            console.log("\nSearch results ("+searcher.results.length+"):");
            for (var r in searcher.results) {
                console.log(searcher.results[r].toString());
            }
        }

        //if (settings.doTiming && settings.debug) {
        //    console.log("\nTimers:");
        //    for (var t in _timers) {
        //        console.log(t + ": " + _timers[t]);
        //    }
        //}
        if (settings.listDirs) {
            searcher.printMatchingDirs();
        }
        if (settings.listFiles) {
            searcher.printMatchingFiles();
        }
        if (settings.listLines) {
            searcher.printMatchingLines();
        }

    } catch (err) {
        console.log('\n' + err + '\n');
        searchOptions.usageWithCode(1);
    }
       
}

// node.js equivalent of python's if __name__ == '__main__'
if (!module.parent) {
    searchMain();
}
