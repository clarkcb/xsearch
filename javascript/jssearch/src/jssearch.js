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

function handleError(err, searchOptions) {
    const errMsg = 'ERROR: ' + err.message;
    common.log('\n' + errMsg + '\n');
    searchOptions.usageWithCode(1);
}

function cmpSearchResults(r1, r2) {
    const pathCmp = r1.file.path.localeCompare(r2.file.path);
    if (pathCmp === 0) {
        const fileCmp = path.basename(r1.file.fileName).localeCompare(path.basename(r2.file.fileName));
        if (fileCmp === 0) {
            if (r1.lineNum === r2.lineNum) {
                return r1.matchStartIndex - r2.matchStartIndex;
            }
            return r1.lineNum - r2.lineNum;
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
    const dirs = results.map(r => r.file.path);
    return common.setFromArray(dirs);
}

function printMatchingDirs(results) {
    const dirs = getMatchingDirs(results);
    if (dirs.length > 0) {
        common.log(`\nMatching directories (${dirs.length}):`);
        dirs.forEach(d => common.log(d));
    } else {
        common.log('\nMatching directories: 0');
    }
}

function getMatchingFiles(results) {
    const files = results.map(r => r.file.relativePath());
    return common.setFromArray(files);
}

function printMatchingFiles(results) {
    const files = getMatchingFiles(results);
    if (files.length > 0) {
        common.log(`\nMatching files (${files.length}):`);
        files.forEach(f => common.log(f));
    } else {
        common.log('\nMatching files: 0');
    }
}

function getMatchingLines(results, uniqueLines) {
    let lines = results.filter(r => r.lineNum > 0).map(r => r.line.trim());
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
        hdrText = `\nUnique lines with matches`;
    else
        hdrText = `\nLines with matches`;
    if (lines.length > 0) {
        hdrText = `${hdrText} (${lines.length}):`;
        common.log(hdrText);
        lines.forEach(l => common.log(l));
    } else {
        hdrText = `${hdrText}: 0`;
        common.log(hdrText);
    }
}

const searchMain = async () => {
    const searchOptions = new SearchOptions();
    const args = process.argv.slice(2);

    searchOptions.settingsFromArgs(args, async (err, settings) => {
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
            const searcher = new Searcher(settings);
            let results = await searcher.search();

            if (settings.printResults) {
                printSearchResults(results, settings);
            }

            if (settings.printDirs) {
                printMatchingDirs(results);
            }
            if (settings.printFiles) {
                printMatchingFiles(results);
            }
            if (settings.printLines) {
                printMatchingLines(results, settings.uniqueLines);
            }

        } catch (err2) {
            handleError(err2, searchOptions);
        }
    });
};

// node.js equivalent of python's if __name__ == '__main__'
if (require.main === module) {
    searchMain().catch((err) => common.log(err));
}
