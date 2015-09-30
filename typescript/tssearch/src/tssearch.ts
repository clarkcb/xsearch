/// <reference path="searcher.ts"/>
/// <reference path="searchoptions.ts"/>
/// <reference path="searchsettings.ts"/>
/*
 * tssearch.js
 *
 * file search utility written in typescript
 */

"use strict";

var common = require('./common.js');
var Searcher = require('./searcher.js').Searcher;
var SearchOptions = require('./searchoptions.js').SearchOptions;

function handleError(err: Error, searchOptions: SearchOptions) {
    var errMsg: string = err.toString().replace(/\w*Error/, "ERROR");
    common.log('\n' + errMsg + '\n');
    searchOptions.usageWithCode(1);
}

function searchMain() {
    var searchOptions = new SearchOptions();

    var args = process.argv.slice(2);

    searchOptions.settingsFromArgs(args, function(err: Error, settings: SearchSettings) {
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
            var searcher: Searcher = new Searcher(settings);
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
