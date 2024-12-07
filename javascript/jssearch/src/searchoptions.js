/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

const {FileUtil} = require('jsfind');
const config = require('./config');
const {SearchError} = require('./searcherror');
const {SearchOption} = require('./searchoption');
const {SearchSettings} = require('./searchsettings');
const {nameToSortBy} = require("../../../../xfind/javascript/jsfind/src/sortby");

class SearchOptions {
    'use strict'

    constructor() {
        this.argNameMap = {};
        this.boolActionMap = {
            'allmatches':
                (b, settings) => { settings.firstMatch = !b; },
            'archivesonly':
                (b, settings) => { settings.archivesOnly = b; },
            'colorize':
                (b, settings) => { settings.colorize = b; },
            'debug':
                (b, settings) => { settings.debug = b; },
            'excludehidden':
                (b, settings) => { settings.includeHidden = !b; },
            'firstmatch':
                (b, settings) => { settings.firstMatch = b; },
            'followsymlinks':
                (b, settings) => { settings.followSymlinks = b; },
            'includehidden':
                (b, settings) => { settings.includeHidden = b; },
            'help':
                (b, settings) => { settings.printUsage = b; },
            'multilinesearch':
                (b, settings) => { settings.multilineSearch = b; },
            'nocolorize':
                (b, settings) => { settings.colorize = !b; },
            'nofollowsymlinks':
                (b, settings) => { settings.followSymlinks = !b; },
            'noprintdirs':
                (b, settings) => { settings.printDirs = !b; },
            'noprintfiles':
                (b, settings) => { settings.printFiles = !b; },
            'noprintlines':
                (b, settings) => { settings.printLines = !b; },
            'noprintmatches':
                (b, settings) => { settings.printResults = !b; },
            'norecursive':
                (b, settings) => { settings.recursive = !b; },
            'nosearcharchives':
                (b, settings) => { settings.searchArchives = !b; },
            'printdirs':
                (b, settings) => { settings.printDirs = b; },
            'printfiles':
                (b, settings) => { settings.printFiles = b; },
            'printlines':
                (b, settings) => { settings.printLines = b; },
            'printmatches':
                (b, settings) => { settings.printResults = b; },
            'recursive':
                (b, settings) => { settings.recursive = b; },
            'searcharchives':
                (b, settings) => { settings.searchArchives = b; },
            'sort-ascending':
                (b, settings) => { settings.sortDescending = !b; },
            'sort-caseinsensitive':
                (b, settings) => { settings.sortCaseInsensitive = b; },
            'sort-casesensitive':
                (b, settings) => { settings.sortCaseInsensitive = !b; },
            'sort-descending':
                (b, settings) => { settings.sortDescending = b; },
            'uniquelines':
                (b, settings) => { settings.uniqueLines = b; },
            'verbose':
                (b, settings) => { settings.verbose = b; },
            'version':
                (b, settings) => { settings.printVersion = b; }
        };
        this.strActionMap = {
            'encoding':
                (s, settings) => { settings.textFileEncoding = s; },
            'in-archiveext':
                (s, settings) => { settings.addInArchiveExtensions(s); },
            'in-archivefilepattern':
                (s, settings) => { settings.addInArchiveFilePatterns(s); },
            'in-dirpattern':
                (s, settings) => { settings.addInDirPatterns(s); },
            'in-ext':
                (s, settings) => { settings.addInExtensions(s); },
            'in-filepattern':
                (s, settings) => { settings.addInFilePatterns(s); },
            'in-filetype':
                (s, settings) => { settings.addInFileTypes(s); },
            'in-linesafterpattern':
                (s, settings) => { settings.addInLinesAfterPatterns(s); },
            'in-linesbeforepattern':
                (s, settings) => { settings.addInLinesBeforePatterns(s); },
            'linesaftertopattern':
                (s, settings) => { settings.addLinesAfterToPatterns(s); },
            'linesafteruntilpattern':
                (s, settings) => { settings.addLinesAfterUntilPatterns(s); },
            'maxlastmod':
                (s, settings) => { settings.maxLastModFromString(s); },
            'minlastmod':
                (s, settings) => { settings.minLastModFromString(s); },
            'out-dirpattern':
                (s, settings) => { settings.addOutDirPatterns(s); },
            'out-archiveext':
                (s, settings) => { settings.addOutArchiveExtensions(s); },
            'out-archivefilepattern':
                (s, settings) => { settings.addOutArchiveFilePatterns(s); },
            'out-ext':
                (s, settings) => { settings.addOutExtensions(s); },
            'out-filepattern':
                (s, settings) => { settings.addOutFilePatterns(s); },
            'out-filetype':
                (s, settings) => { settings.addOutFileTypes(s); },
            'out-linesafterpattern':
                (s, settings) => { settings.addOutLinesAfterPatterns(s); },
            'out-linesbeforepattern':
                (s, settings) => { settings.addOutLinesBeforePatterns(s); },
            'path':
                (s, settings) => { settings.paths.push(s); },
            'searchpattern':
                (s, settings) => { settings.addSearchPatterns(s); },
            'settings-file':
                (s, settings) => { return this.settingsFromFile(s, settings); },
            'sort-by':
                (s, settings) => { settings.sortBy = nameToSortBy(s); }
        };
        this.intActionMap = {
            'linesafter':
                (i, settings) => { settings.linesAfter = i; },
            'linesbefore':
                (i, settings) => { settings.linesBefore = i; },
            'maxdepth':
                (i, settings) => { settings.maxDepth = i; },
            'maxlinelength':
                (i, settings) => { settings.maxLineLength = i; },
            'maxsize':
                (i, settings) => { settings.maxSize = i; },
            'mindepth':
                (i, settings) => { settings.minDepth = i; },
            'minsize':
                (i, settings) => { settings.minSize = i; },
        };

        // the list of SearchOption objects (populated from JSON)
        this.options = [];
        (() => {
            let json = FileUtil.getFileContentsSync(config.SEARCH_OPTIONS_JSON_PATH, 'utf-8');
            let obj = JSON.parse(json);
            if (Object.prototype.hasOwnProperty.call(obj, 'searchoptions') && Array.isArray(obj.searchoptions)) {
                obj.searchoptions.forEach(so => {
                    let longArg = so.long;
                    let shortArg = '';
                    if (Object.prototype.hasOwnProperty.call(so, 'short'))
                        shortArg = so.short;
                    let desc = so.desc;
                    this.argNameMap[longArg] = longArg;
                    if (shortArg) this.argNameMap[shortArg] = longArg;
                    const option = new SearchOption(shortArg, longArg, desc);
                    this.options.push(option);
                });
            } else throw new SearchError(`Invalid searchoptions file: ${config.SEARCH_OPTIONS_JSON_PATH}`);
            this.options.sort(this.optCmp);
        })();

    }

    optCmp(o1, o2) {
        const a = o1.sortArg;
        const b = o2.sortArg;
        return a.localeCompare(b);
    }

    settingsFromFile(filePath, settings) {
        const fs = require('fs');
        if (fs.existsSync(filePath)) {
            let json = FileUtil.getFileContentsSync(filePath, 'utf-8');
            return this.settingsFromJson(json, settings);
        } else {
            return new SearchError('Settings file not found');
        }
    }

    settingsFromJson(json, settings) {
        let err = null;
        let obj = JSON.parse(json);
        for (let k in obj) {
            if (err) break;
            if (obj.hasOwnProperty(k)) {
                let longKey = this.argNameMap[k];
                if (this.boolActionMap[longKey]) {
                    this.boolActionMap[longKey](obj[k], settings);
                } else if (this.strActionMap[k]) {
                    if (obj[k]) {
                        this.strActionMap[k](obj[k], settings);
                    } else {
                        err = new Error("Missing argument for option " + k);
                    }
                } else if (this.intActionMap[longKey]) {
                    this.intActionMap[longKey](obj[k], settings);
                } else if (k === 'path') {
                    settings.paths.push(obj[k]);
                } else {
                    err = new SearchError("Invalid option: " + k);
                }
            }
        }
        return err;
    }

    settingsFromArgs(args, cb) {
        let err = null;
        let settings = new SearchSettings();
        // default printResults to true since running as cli
        settings.printResults = true;

        while(args && !err) {
            let arg = args.shift();
            if (!arg) {
                break;
            }
            if (arg.charAt(0) === '-') {
                while (arg && arg.charAt(0) === '-') {
                    arg = arg.substring(1);
                }
                let longArg = this.argNameMap[arg];
                if (this.boolActionMap[longArg]) {
                    this.boolActionMap[longArg](true, settings);
                } else if (this.strActionMap[longArg] || this.intActionMap[longArg]) {
                    if (args.length > 0) {
                        if (this.strActionMap[longArg]) {
                            err = this.strActionMap[longArg](args.shift(), settings);
                        } else {
                            err = this.intActionMap[longArg](parseInt(args.shift(), 10), settings);
                        }
                    } else {
                        err = new Error(`Missing argument for option ${arg}`);
                    }
                } else {
                    err = new Error(`Invalid option: ${arg}`);
                }
            } else {
                settings.paths.push(arg);
            }
        }
        cb(err, settings);
    }

    usage() {
        this.usageWithCode(0);
    }

    usageWithCode(exitCode) {
        console.log(this.getUsageString());
        process.exit(exitCode);
    }

    getUsageString() {
        let usage = 'Usage:\n jssearch [options] -s <searchpattern> <path> [<path> ...]\n\n';
        usage += 'Options:\n';
        let optStrings = [];
        let optDescs = [];
        let longest = 0;
        this.options.forEach(opt => {
            let optString = ' ';
            if (opt.shortArg)
                optString += '-' + opt.shortArg + ',';
            optString += '--' + opt.longArg;
            if (optString.length > longest)
                longest = optString.length;
            optStrings.push(optString);
            optDescs.push(opt.desc);
        });
        for (let i=0; i < optStrings.length; i++) {
            let os = optStrings[i];
            while (os.length < longest)
                os += ' ';
            usage += os + '  ' + optDescs[i] + '\n';
        }
        return usage;
    }
}

exports.SearchOptions = SearchOptions;
