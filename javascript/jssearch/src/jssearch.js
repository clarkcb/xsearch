/*
 * jssearch.js
 *
 * file search utility written in JavaScript + Node.js
 */

"use strict";

var common = require('./common.js');
var Searcher = require('./searcher.js').Searcher;
var SearchOptions = require('./searchoptions.js').SearchOptions;

function handleError(err, searchOptions) {
    const errMsg = err.toString().replace(/\w*Error/, "ERROR");
    common.log('\n' + errMsg + '\n');
    searchOptions.usageWithCode(1);
}

function searchMain() {
    const searchOptions = new SearchOptions();
    const args = process.argv.slice(2);

    searchOptions.settingsFromArgs(args, (err, settings) => {
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
            let searcher = new Searcher(settings);
            searcher.search();

            if (settings.printResults) {
                searcher.printSearchResults();
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
