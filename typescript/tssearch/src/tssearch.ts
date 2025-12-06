/*
 * tssearch.ts
 *
 * file search utility written in typescript
 */

'use strict';

import * as common from './common';
import {Searcher} from './searcher';
import {SearchOptions} from './searchoptions';
import {SearchSettings} from './searchsettings';
import {SearchResultFormatter} from "./searchresultformatter";

function handleError(err: Error | any, colorize: boolean, searchOptions: SearchOptions) {
    const errMsg: string = 'ERROR: ' + err.message;
    common.logError('\n' + errMsg + '\n', colorize);
    searchOptions.usageWithCode(1);
}

const searchMain = async () => {
    const searchOptions = new SearchOptions();
    const args = process.argv.slice(2);

    searchOptions.settingsFromArgs(args, async (err: Error | void, settings: SearchSettings) => {
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
            const searcher: Searcher = new Searcher(settings);
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

        } catch (err2) {
            handleError(err2, settings.colorize, searchOptions);
        }
    });
}

// node.js equivalent of python's if __name__ == '__main__'
if (require.main === module) {
    searchMain().catch((err) => common.logError(err));
}
