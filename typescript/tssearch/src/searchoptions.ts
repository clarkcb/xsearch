/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

"use strict";

import * as config from './config';
import {FileUtil, FindSettings, SortUtil} from 'tsfind';
import {SearchOption} from './searchoption';
import {SearchSettings} from './searchsettings';

interface StringOptionMap {
    [key: string]: SearchOption
}

interface StringActionMap {
    [key: string]: any
}

export class SearchOptions {
    // the list of SearchOption objects (populated by setOptionsFromXml)
    options: SearchOption[];
    argNameMap: {[index: string]:string};
    argMap: StringOptionMap;
    flagMap: StringOptionMap;
    argActionMap: StringActionMap;
    boolFlagActionMap: StringActionMap;

    constructor() {
        this.options = [];
        this.argNameMap = {};
        this.argMap = {};
        this.flagMap = {};

        this.argActionMap = {
            'encoding':
                (x: string, settings: SearchSettings) => { settings.textFileEncoding = x; },
            'in-archiveext':
                (x: string, settings: SearchSettings) => { settings.addInArchiveExtensions(x); },
            'in-archivefilepattern':
                (x: string, settings: SearchSettings) => { settings.addInArchiveFilePatterns(x); },
            'in-dirpattern':
                (x: string, settings: SearchSettings) => { settings.addInDirPatterns(x); },
            'in-ext':
                (x: string, settings: SearchSettings) => { settings.addInExtensions(x); },
            'in-filepattern':
                (x: string, settings: SearchSettings) => { settings.addInFilePatterns(x); },
            'in-filetype':
                (x: string, settings: SearchSettings) => { settings.addInFileTypes(x); },
            'in-linesafterpattern':
                (x: string, settings: SearchSettings) => { settings.addInLinesAfterPatterns(x); },
            'in-linesbeforepattern':
                (x: string, settings: SearchSettings) => { settings.addInLinesBeforePatterns(x); },
            'linesafter':
                (x: string, settings: SearchSettings) => { settings.linesAfter = parseInt(x); },
            'linesaftertopattern':
                (x: string, settings: SearchSettings) => { settings.addLinesAfterToPatterns(x); },
            'linesafteruntilpattern':
                (x: string, settings: SearchSettings) => { settings.addLinesAfterUntilPatterns(x); },
            'linesbefore':
                (x: string, settings: SearchSettings) => { settings.linesBefore = parseInt(x); },
            'maxdepth':
                (x: string, settings: SearchSettings) => { settings.maxDepth = parseInt(x, 10); },
            'maxlastmod':
                (x: string, settings: SearchSettings) => { settings.maxLastModFromString(x); },
            'maxlinelength':
                (x: string, settings: SearchSettings) => { settings.maxLineLength = parseInt(x); },
            'maxsize':
                (x: string, settings: SearchSettings) => { settings.maxSize = parseInt(x, 10); },
            'mindepth':
                (x: string, settings: SearchSettings) => { settings.minDepth = parseInt(x, 10); },
            'minlastmod':
                (x: string, settings: SearchSettings) => { settings.minLastModFromString(x); },
            'minsize':
                (x: string, settings: SearchSettings) => { settings.minSize = parseInt(x, 10); },
            'out-dirpattern':
                (x: string, settings: SearchSettings) => { settings.addOutDirPatterns(x); },
            'out-archiveext':
                (x: string, settings: SearchSettings) => { settings.addOutArchiveExtensions(x); },
            'out-archivefilepattern':
                (x: string, settings: SearchSettings) => { settings.addOutArchiveFilePatterns(x); },
            'out-ext':
                (x: string, settings: SearchSettings) => { settings.addOutExtensions(x); },
            'out-filepattern':
                (x: string, settings: SearchSettings) => { settings.addOutFilePatterns(x); },
            'out-filetype':
                (x: string, settings: SearchSettings) => { settings.addOutFileTypes(x); },
            'out-linesafterpattern':
                (x: string, settings: SearchSettings) => { settings.addOutLinesAfterPatterns(x); },
            'out-linesbeforepattern':
                (x: string, settings: SearchSettings) => { settings.addOutLinesBeforePatterns(x); },
            'path':
                (x: string, settings: SearchSettings) => { settings.paths.push(x); },
            'searchpattern':
                (x: string, settings: SearchSettings) => { settings.addSearchPatterns(x); },
            'settings-file':
                (x: string, settings: SearchSettings) => { this.settingsFromFile(x, settings); },
            'sort-by':
                (x: string, settings: SearchSettings) => { settings.sortBy = SortUtil.nameToSortBy(x); }

        };

        this.boolFlagActionMap = {
            'allmatches':
                (b: boolean, settings: SearchSettings) => { settings.firstMatch = !b; },
            'archivesonly':
                (b: boolean, settings: SearchSettings) => { settings.archivesOnly = b; },
            'debug':
                (b: boolean, settings: SearchSettings) => { settings.debug = b; },
            'excludehidden':
                (b: boolean, settings: SearchSettings) => { settings.includeHidden = !b; },
            'firstmatch':
                (b: boolean, settings: SearchSettings) => { settings.firstMatch = b; },
            'followsymlinks':
                (b: boolean, settings: SearchSettings) => { settings.followSymlinks = b; },
            'includehidden':
                (b: boolean, settings: SearchSettings) => { settings.includeHidden = b; },
            'help':
                (b: boolean, settings: SearchSettings) => { settings.printUsage = b; },
            'multilinesearch':
                (b: boolean, settings: SearchSettings) => { settings.multilineSearch = b; },
            'nofollowsymlinks':
                (b: boolean, settings: SearchSettings) => { settings.followSymlinks = !b; },
            'noprintdirs':
                (b: boolean, settings: SearchSettings) => { settings.printDirs = !b; },
            'noprintfiles':
                (b: boolean, settings: SearchSettings) => { settings.printFiles = !b; },
            'noprintlines':
                (b: boolean, settings: SearchSettings) => { settings.printLines = !b; },
            'noprintmatches':
                (b: boolean, settings: SearchSettings) => { settings.printResults = !b; },
            'norecursive':
                (b: boolean, settings: SearchSettings) => { settings.recursive = !b; },
            'nosearcharchives':
                (b: boolean, settings: SearchSettings) => { settings.searchArchives = !b; },
            'printdirs':
                (b: boolean, settings: SearchSettings) => { settings.printDirs = b; },
            'printfiles':
                (b: boolean, settings: SearchSettings) => { settings.printFiles = b; },
            'printlines':
                (b: boolean, settings: SearchSettings) => { settings.printLines = b; },
            'printmatches':
                (b: boolean, settings: SearchSettings) => { settings.printResults = b; },
            'recursive':
                (b: boolean, settings: SearchSettings) => { settings.recursive = b; },
            'searcharchives':
                (b: boolean, settings: SearchSettings) => { settings.searchArchives = b; },
            'sort-ascending':
                (b: boolean, settings: SearchSettings) => { settings.sortDescending = !b; },
            'sort-caseinsensitive':
                (b: boolean, settings: SearchSettings) => { settings.sortCaseInsensitive = b; },
            'sort-casesensitive':
                (b: boolean, settings: SearchSettings) => { settings.sortCaseInsensitive = !b; },
            'sort-descending':
                (b: boolean, settings: SearchSettings) => { settings.sortDescending = b; },
            'uniquelines':
                (b: boolean, settings: SearchSettings) => { settings.uniqueLines = b; },
            'verbose':
                (b: boolean, settings: SearchSettings) => { settings.verbose = b; },
            'version':
                (b: boolean, settings: SearchSettings) => { settings.printVersion = b; }
        };

        this.setOptionsFromJsonFile();
    }

    private static optCmp(o1: SearchOption, o2: SearchOption) {
        const a: string = o1.sortArg;
        const b: string = o2.sortArg;
        return a.localeCompare(b);
    }

    // setOptionsFromJsonFile
    private setOptionsFromJsonFile(): void {
        const fs = require('fs');

        let json = '';
        if (fs.existsSync(FileUtil.expandPath(config.SEARCHOPTIONSJSONPATH))) {
            json = fs.readFileSync(FileUtil.expandPath(config.SEARCHOPTIONSJSONPATH)).toString();
        } else {
            throw new Error('File not found: ' + config.SEARCHOPTIONSJSONPATH);
        }

        const obj = JSON.parse(json);
        if (obj.hasOwnProperty('searchoptions') && Array.isArray(obj['searchoptions'])) {
            obj['searchoptions'].forEach(so => {
                const longArg = so['long'];
                let shortArg = '';
                if (so.hasOwnProperty('short'))
                    shortArg = so['short'];
                const desc = so['desc'];
                this.argNameMap[longArg] = longArg;
                if (shortArg) this.argNameMap[shortArg] = longArg;
                const option = new SearchOption(shortArg, longArg, desc);
                this.options.push(option);
                if (this.argActionMap[longArg]) {
                    this.argMap[longArg] = option;
                    if (shortArg) this.argMap[shortArg] = option;
                } else if (this.boolFlagActionMap[longArg]) {
                    this.flagMap[longArg] = option;
                    if (shortArg) this.flagMap[shortArg] = option;
                }
            });
        } else throw new Error("Invalid searchoptions file: " + config.SEARCHOPTIONSJSONPATH);
        this.options.sort(SearchOptions.optCmp);
    }

    private settingsFromFile(filepath: string, settings: SearchSettings): Error | undefined {
        const fs = require('fs');
        if (fs.existsSync(filepath)) {
            const json: string = FileUtil.getFileContentsSync(filepath, settings.textFileEncoding);
            return this.settingsFromJson(json, settings);
        } else {
            return new Error('Settings file not found');
        }
    }

    public settingsFromJson(json: string, settings: SearchSettings): Error | undefined {
        let err: Error | undefined = undefined;
        const obj = JSON.parse(json);
        for (const k in obj) {
            if (err) break;
            if (obj.hasOwnProperty(k)) {
                if (this.argMap[k]) {
                    if (obj[k]) {
                        err = this.argActionMap[k](obj[k], settings);
                    } else {
                        err = new Error("Missing argument for option "+k);
                    }
                } else if (this.boolFlagActionMap[k]) {
                    this.boolFlagActionMap[k](obj[k], settings);
                } else if (k == 'path') {
                    settings.paths.push(obj[k]);
                } else {
                    err = new Error("Invalid option: "+k);
                }
            }
        }
        return err;
    }

    public settingsFromArgs(args: string[], cb: (err: Error | undefined, settings: SearchSettings) => void) {
        let err: Error | undefined = undefined;
        const settings: SearchSettings = new SearchSettings();
        // default printResults to true since it's being run from cmd line
        settings.printResults = true;
        while(args && !err) {
            let arg: string = args.shift() || '';
            if (!arg) {
                break;
            }
            if (arg.charAt(0) === '-') {
                while (arg && arg.charAt(0) === '-') {
                    arg = arg.substring(1);
                }
                const longArg = this.argNameMap[arg];
                if (this.argMap[longArg]) {
                    if (args.length > 0) {
                        err = this.argActionMap[longArg](args.shift(), settings);
                    } else {
                        err = new Error("Missing argument for option " + arg);
                    }
                } else if (this.flagMap[longArg]) {
                    this.boolFlagActionMap[longArg](true, settings);
                } else {
                    err = new Error("Invalid option: " + arg);
                }
            } else {
                settings.paths.push(arg);
            }
        }
        cb(err, settings);
    }

    public usage(): void {
        this.usageWithCode(0);
    }

    public usageWithCode(exitCode: number): void {
        console.log(this.getUsageString());
        process.exit(exitCode);
    }

    private getUsageString(): string {
        let usage: string = 'Usage:\n tssearch [options] -s <searchpattern>' +
            ' <path> [<path> ...]\n\nOptions:\n';
        const optStrings: string[] = [];
        const optDescs: string[] = [];
        let longest = 0;
        this.options.forEach((opt: SearchOption) => {
            let optString = ' ';
            if (opt.shortArg)
                optString += '-' + opt.shortArg + ',';
            optString += '--' + opt.longArg;
            if (optString.length > longest)
                longest = optString.length;
            optStrings.push(optString);
            optDescs.push(opt.desc);
        });
        for (let i = 0; i < optStrings.length; i++) {
            let os: string = optStrings[i];
            while (os.length < longest)
                os += ' ';
            usage += os + '  ' + optDescs[i] + '\n';
        }
        return usage;
    }
}
