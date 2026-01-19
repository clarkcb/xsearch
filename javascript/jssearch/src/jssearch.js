/*
 * jssearch.js
 *
 * file search utility written in JavaScript + Node.js
 */

'use strict';

const path = require('path');

const {common} = require('jsfind');
const {Searcher} = require('./searcher');
const {SearchOptions} = require('./searchoptions');
const {SearchResultFormatter} = require('./searchresultformatter');

function handleError(err, colorize, searchOptions) {
    const errMsg = 'ERROR: ' + err.message;
    common.logError('\n' + errMsg + '\n', colorize);
    searchOptions.usageWithCode(1);
}

const searchMain = async () => {
    const searchOptions = new SearchOptions();
    const args = process.argv.slice(2);

    searchOptions.settingsFromArgs(args, async (err, settings) => {
        if (err) {
            handleError(err, true, searchOptions);
        }

        if (settings.debug)
            common.log('settings: ' + settings.toString());

        if (settings.printUsage) {
            common.log('');
            searchOptions.usage();
        }

        if (settings.printVersion) {
            common.log('Version: 0.1');
            process.exit(0);
        }

        try {
            const searcher = new Searcher(settings);
            const results = await searcher.search();
            const formatter = new SearchResultFormatter(settings);

            if (settings.printResults) {
                searcher.printSearchResults(results, formatter);
            }

            if (settings.printDirs) {
                searcher.printMatchingDirs(results, formatter);
            }
            if (settings.printFiles) {
                searcher.printMatchingFiles(results, formatter);
            }
            if (settings.printLines) {
                searcher.printMatchingLines(results, formatter);
            }
            if (settings.printMatches) {
                searcher.printMatches(results, formatter);
            }

        } catch (err2) {
            handleError(err2, settings.colorize, searchOptions);
        }
    });
};

// node.js equivalent of python's if __name__ == '__main__'
if (require.main === module) {
    searchMain().catch((err) => common.log(err));
}
