/*
 * nodesearch.js
 *
 * file search utility written in node.js
 */

var Searcher = require('./searcher.js').Searcher;
var SearchOptions = require('./searchoptions.js').SearchOptions;

function log(message) {
    console.log(message);
}

function handleError(err, searchOptions) {
    log('\n' + err + '\n');
    searchOptions.usageWithCode(1);
}

function searchMain() {
    var searchOptions = new SearchOptions();

    var args = process.argv.slice(2);
    //log('args: ', args);

    searchOptions.searchSettingsFromArgs(args, function(err, settings) {
        if (err) {
            handleError(err, searchOptions);
        }

        if (settings.printUsage)
            searchOptions.usage();

        if (settings.printVersion) {
            log('Version: 0.1');
            process.exit(0);
        }

        if (settings.debug)
            log("settings: " + settings.toString());

         try {
            var searcher = new Searcher(settings);
            searcher.search();

            if (settings.printResults) {
                log("\nSearch results ("+searcher.results.length+"):");
                for (var r in searcher.results) {
                    log(searcher.results[r].toString());
                }
            }

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
            handleError(err, searchOptions);
        }
    });
}

// node.js equivalent of python's if __name__ == '__main__'
if (!module.parent) {
    searchMain();
}
