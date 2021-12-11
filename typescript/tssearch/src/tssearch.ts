/*
 * tssearch.ts
 *
 * file search utility written in typescript
 */

"use strict";

import * as common from './common';
import {SearchOptions} from './searchoptions';
import {SearchSettings} from './searchsettings';
import {Searcher} from './searcher';

function handleError(err: Error | any, searchOptions: SearchOptions) {
    const errMsg: string = 'ERROR: ' + err.message;
    common.log('\n' + errMsg + '\n');
    searchOptions.usageWithCode(1);
}

function searchMain() {
    const searchOptions = new SearchOptions();
    const args = process.argv.slice(2);

    searchOptions.settingsFromArgs(args, async (err: Error | void, settings: SearchSettings) => {
        if (err) {
            handleError(err, searchOptions);
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
            await searcher.search();

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
if (require.main === module) {
    searchMain();
}
