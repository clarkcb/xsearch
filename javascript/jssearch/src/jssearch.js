/*
 * jssearch.js
 *
 * file search utility written in JavaScript + Node.js
 */

"use strict";

const path = require('path');

const common = require('./common');
const {Searcher} = require('./searcher');
const {SearchError} = require('./searcherror');
const {SearchOptions} = require('./searchoptions');
const {SearchResultFormatter} = require('./searchresultformatter');

function handleError(err, searchOptions) {
    const errMsg = "ERROR: " + err.message;
    common.log('\n' + errMsg + '\n');
    searchOptions.usageWithCode(1);
}

function cmpSearchResults(r1, r2) {
    const pathCmp = r1.file.pathname.localeCompare(r2.file.pathname);
    if (pathCmp === 0) {
        const fileCmp = path.basename(r1.file.filename).localeCompare(path.basename(r2.file.filename));
        if (fileCmp === 0) {
            if (r1.linenum === r2.linenum) {
                return r1.matchStartIndex - r2.matchStartIndex;
            }
            return r1.linenum - r2.linenum;
        }
        return fileCmp;
    }
    return pathCmp;
}

function printSearchResults(results, settings) {
    // first sort the results
    results.sort(cmpSearchResults);
    const formatter = new SearchResultFormatter(settings);
    common.log(`\nSearch results (${results.length}):`);
    results.forEach(r => common.log(formatter.format(r)));
}

function getMatchingDirs(results) {
    const dirs = results.map(r => path.dirname(r.filename));
    return common.setFromArray(dirs);
}

function printMatchingDirs(results) {
    const dirs = getMatchingDirs(results);
    common.log(`\nDirectories with matches (${dirs.length}):`);
    dirs.forEach(d => common.log(d));
}

function getMatchingFiles(results) {
    const files = results.map(r => r.filename);
    return common.setFromArray(files);
}

function printMatchingFiles(results) {
    const files = getMatchingFiles(results);
    common.log(`\nFiles with matches (${files.length}):`);
    files.forEach(f => common.log(f));
}

function getMatchingLines(results, uniqueLines) {
    let lines = results.filter(r => r.linenum > 0).map(r => r.line.trim());
    if (uniqueLines) {
        lines = common.setFromArray(lines);
    }
    lines.sort((a, b) => {
        if (a.toUpperCase() === b.toUpperCase())
            return 0;
        return a.toUpperCase() < b.toUpperCase() ? -1 : 1;
    });
    return lines;
}

function printMatchingLines(results, uniqueLines) {
    const lines = getMatchingLines(results);
    let hdrText;
    if (uniqueLines)
        hdrText = `\nUnique lines with matches (${lines.length}):`;
    else
        hdrText = `\nLines with matches (${lines.length}):`;
    common.log(hdrText);
    lines.forEach(l => common.log(l));
}

const searchMain = async () => {
    const searchOptions = new SearchOptions();
    const args = process.argv.slice(2);

    searchOptions.settingsFromArgs(args, async (err, settings) => {
        if (err) {
            handleError(err, searchOptions);
        }

        if (settings.debug)
            common.log("settings: " + settings.toString());

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
            let results = await searcher.search();

            if (settings.printResults) {
                printSearchResults(results, settings);
            }

            if (settings.listDirs) {
                printMatchingDirs(results);
            }
            if (settings.listFiles) {
                printMatchingFiles(results);
            }
            if (settings.listLines) {
                printMatchingLines(results, settings.uniqueLines);
            }

        } catch (err2) {
            handleError(err2, searchOptions);
        }
    });
};

// node.js equivalent of python's if __name__ == '__main__'
if (!module.parent) {
    searchMain();
}
