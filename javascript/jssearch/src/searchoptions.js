/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

const config = require('./config');
const {expandPath} = require('./fileutil');
const {SearchError} = require('./searcherror');
const {SearchOption} = require('./searchoption');
const {SearchSettings} = require('./searchsettings');

class SearchOptions {
    'use strict'

    constructor() {
        this.argNameMap = {};
        this.argMap = {};
        this.flagMap = {};
        this.argActionMap = {
            'encoding':
                (x, settings) => { settings.textFileEncoding = x; },
            'in-archiveext':
                (x, settings) => { settings.addInArchiveExtensions(x); },
            'in-archivefilepattern':
                (x, settings) => { settings.addInArchiveFilePatterns(x); },
            'in-dirpattern':
                (x, settings) => { settings.addInDirPatterns(x); },
            'in-ext':
                (x, settings) => { settings.addInExtensions(x); },
            'in-filepattern':
                (x, settings) => { settings.addInFilePatterns(x); },
            'in-filetype':
                (x, settings) => { settings.addInFileTypes(x); },
            'in-linesafterpattern':
                (x, settings) => { settings.addInLinesAfterPatterns(x); },
            'in-linesbeforepattern':
                (x, settings) => { settings.addInLinesBeforePatterns(x); },
            'linesafter':
                (x, settings) => { settings.linesAfter = parseInt(x); },
            'linesaftertopattern':
                (x, settings) => { settings.addLinesAfterToPatterns(x); },
            'linesafteruntilpattern':
                (x, settings) => { settings.addLinesAfterUntilPatterns(x); },
            'linesbefore':
                (x, settings) => { settings.linesBefore = parseInt(x); },
            'maxlinelength':
                (x, settings) => { settings.maxLineLength = parseInt(x); },
            'out-dirpattern':
                (x, settings) => { settings.addOutDirPatterns(x); },
            'out-archiveext':
                (x, settings) => { settings.addOutArchiveExtensions(x); },
            'out-archivefilepattern':
                (x, settings) => { settings.addOutArchiveFilePatterns(x); },
            'out-ext':
                (x, settings) => { settings.addOutExtensions(x); },
            'out-filepattern':
                (x, settings) => { settings.addOutFilePatterns(x); },
            'out-filetype':
                (x, settings) => { settings.addOutFileTypes(x); },
            'out-linesafterpattern':
                (x, settings) => { settings.addOutLinesAfterPatterns(x); },
            'out-linesbeforepattern':
                (x, settings) => { settings.addOutLinesBeforePatterns(x); },
            'searchpattern':
                (x, settings) => { settings.addSearchPatterns(x); },
            'settings-file':
                (x, settings) => { return settingsFromFile(x, settings); }

        };
        this.boolFlagActionMap = {
            'allmatches':
                (b, settings) => { settings.firstMatch = !b; },
            'archivesonly':
                (b, settings) => { settings.setArchivesOnly(b); },
            'colorize':
                (b, settings) => { settings.colorize = b; },
            'debug':
                (b, settings) => { settings.setDebug(b); },
            'excludehidden':
                (b, settings) => { settings.excludeHidden = b; },
            'firstmatch':
                (b, settings) => { settings.firstMatch = b; },
            'includehidden':
                (b, settings) => { settings.excludeHidden = !b; },
            'help':
                (b, settings) => { settings.printUsage = b; },
            'listdirs':
                (b, settings) => { settings.listDirs = b; },
            'listfiles':
                (b, settings) => { settings.listFiles = b; },
            'listlines':
                (b, settings) => { settings.listLines = b; },
            'multilinesearch':
                (b, settings) => { settings.multilineSearch = b; },
            'nocolorize':
                (b, settings) => { settings.colorize = !b; },
            'noprintmatches':
                (b, settings) => { settings.printResults = !b; },
            'norecursive':
                (b, settings) => { settings.recursive = !b; },
            'nosearcharchives':
                (b, settings) => { settings.searchArchives = !b; },
            'printmatches':
                (b, settings) => { settings.printResults = b; },
            'recursive':
                (b, settings) => { settings.recursive = b; },
            'searcharchives':
                (b, settings) => { settings.searchArchives = b; },
            'uniquelines':
                (b, settings) => { settings.uniqueLines = b; },
            'verbose':
                (b, settings) => { settings.verbose = b; },
            'version':
                (b, settings) => { settings.printVersion = b; }
        };
        // the list of SearchOption objects (populated from JSON)
        this.options = [];
        (() => {
            const fs = require('fs');

            let json = '';
            if (fs.existsSync(expandPath(config.SEARCHOPTIONSJSONPATH))) {
                json = fs.readFileSync(expandPath(config.SEARCHOPTIONSJSONPATH)).toString();
            } else {
                throw new SearchError('File not found: ' + config.SEARCHOPTIONSJSONPATH);
            }

            let obj = JSON.parse(json);
            if (obj.hasOwnProperty('searchoptions') && Array.isArray(obj['searchoptions'])) {
                obj['searchoptions'].forEach(so => {
                    let longArg = so['long'];
                    let shortArg = '';
                    if (so.hasOwnProperty('short'))
                        shortArg = so['short'];
                    let desc = so['desc'];
                    let func = null;
                    this.argNameMap[longArg] = longArg;
                    if (shortArg) this.argNameMap[shortArg] = longArg;
                    if (this.argActionMap[longArg]) func = this.argActionMap[longArg];
                    else if (this.boolFlagActionMap[longArg]) func = this.boolFlagActionMap[longArg];
                    else throw new SearchError("Unknown option: " + longArg);
                    const option = new SearchOption(shortArg, longArg, desc, func);
                    this.options.push(option);
                    if (this.argActionMap[longArg]) {
                        this.argMap[longArg] = option;
                        if (shortArg) this.argMap[shortArg] = option;
                    } else if (this.boolFlagActionMap[longArg]) {
                        this.flagMap[longArg] = option;
                        if (shortArg) this.flagMap[shortArg] = option;
                    }
                });
            } else throw new SearchError("Invalid searchoptions file: " + config.SEARCHOPTIONSJSONPATH);
            this.options.sort(this.optcmp);
        })();

    }

    optcmp(o1, o2) {
        const a = o1.sortarg;
        const b = o2.sortarg;
        return a.localeCompare(b);
    }

    settingsFromFile(filepath, settings) {
        const fs = require('fs');
        if (fs.existsSync(filepath)) {
            let json = fs.readFileSync(filepath).toString();
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
                if (this.argMap[k]) {
                    if (obj[k]) {
                        this.argMap[k].func(obj[k], settings);
                    } else {
                        err = new Error("Missing argument for option " + k);
                    }
                } else if (this.boolFlagActionMap[longKey]) {
                    this.boolFlagActionMap[longKey](obj[k], settings);
                } else if (k === 'startpath') {
                    settings.startPath = obj[k];
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

        // default printResults to true since it's being run from cmd line
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
                if (this.argMap[arg]) {
                    if (args.length > 0) {
                        err = this.argMap[arg].func(args.shift(), settings);
                    } else {
                        err = new Error("Missing argument for option " + arg);
                    }
                } else if (this.flagMap[arg]) {
                    this.flagMap[arg].func(true, settings);
                } else {
                    err = new Error("Invalid option: " + arg);
                }
            } else {
                settings.startPath = arg;
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
        let usage = 'Usage:\n jssearch [options] -s <searchpattern> <startpath>\n\n';
        usage += 'Options:\n';
        let optStrings = [];
        let optDescs = [];
        let longest = 0;
        this.options.forEach(opt => {
            let optString = ' ';
            if (opt.shortarg)
                optString += '-' + opt.shortarg + ',';
            optString += '--' + opt.longarg;
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
