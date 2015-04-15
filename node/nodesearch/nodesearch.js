/*
 * nodesearch.js
 *
 * file search utility written in node.js
 */

var common = require('./common.js');
var Searcher = require('./searcher.js').Searcher;
var SearchOptions = require('./searchoptions.js').SearchOptions;

function handleError(err, searchOptions) {
    common.log('\n' + err + '\n');
    searchOptions.usageWithCode(1);
}

function searchMain() {
    var searchOptions = new SearchOptions();

    var args = process.argv.slice(2);

    searchOptions.settingsFromArgs(args, function(err, settings) {
        if (err) {
            handleError(err, searchOptions);
        }

        if (settings.printUsage) {
            common.log('');
            searchOptions.usage();
        }

        if (settings.printVersion) {
            common.log('Version: 0.1');
            process.exit(0);
        }

        if (settings.debug)
            common.log("settings: " + settings.toString());

         try {
            var searcher = new Searcher(settings);
            searcher.search();

            if (settings.printResults) {
                common.log("\nSearch results ({0}):".format(searcher.results.length));
                for (var r in searcher.results) {
                    common.log(searcher.results[r].toString());
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

        } catch (err2) {
            handleError(err2, searchOptions);
        }
    });
}

// node.js equivalent of python's if __name__ == '__main__'
if (!module.parent) {
    searchMain();
}
