/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

'use strict';

import * as fs from 'fs';

import * as config from './config';
import {FileUtil, SortUtil} from 'tsfind';
import {SearchError} from './searcherror';
import {SearchOption} from './searchoption';
import {SearchSettings} from './searchsettings';

interface StringActionMap {
    [key: string]: any
}

export class SearchOptions {
    options: SearchOption[];
    argNameMap: {[index: string]:string};
    boolActionMap: StringActionMap;
    stringActionMap: StringActionMap;
    intActionMap: StringActionMap;

    constructor() {
        this.options = [];
        // path included separately because it is not included as an option in findoptions.json
        this.argNameMap = {'path': 'path'};

        this.boolActionMap = {
            'allmatches':
                (b: boolean, settings: SearchSettings) => { settings.firstMatch = !b; },
            'archivesonly':
                (b: boolean, settings: SearchSettings) => { settings.archivesOnly = b; },
            'colorize':
                (b: boolean, settings: SearchSettings) => { settings.colorize = b; },
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
            'nocolorize':
                (b: boolean, settings: SearchSettings) => { settings.colorize = !b; },
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

        this.stringActionMap = {
            'encoding':
                (s: string, settings: SearchSettings) => { settings.textFileEncoding = s; },
            'in-archiveext':
                (s: string, settings: SearchSettings) => { settings.addInArchiveExtensions(s); },
            'in-archivefilepattern':
                (s: string, settings: SearchSettings) => { settings.addInArchiveFilePatterns(s); },
            'in-dirpattern':
                (s: string, settings: SearchSettings) => { settings.addInDirPatterns(s); },
            'in-ext':
                (s: string, settings: SearchSettings) => { settings.addInExtensions(s); },
            'in-filepattern':
                (s: string, settings: SearchSettings) => { settings.addInFilePatterns(s); },
            'in-filetype':
                (s: string, settings: SearchSettings) => { settings.addInFileTypes(s); },
            'in-linesafterpattern':
                (s: string, settings: SearchSettings) => { settings.addInLinesAfterPatterns(s); },
            'in-linesbeforepattern':
                (s: string, settings: SearchSettings) => { settings.addInLinesBeforePatterns(s); },
            'linesaftertopattern':
                (s: string, settings: SearchSettings) => { settings.addLinesAfterToPatterns(s); },
            'linesafteruntilpattern':
                (s: string, settings: SearchSettings) => { settings.addLinesAfterUntilPatterns(s); },
            'maxlastmod':
                (s: string, settings: SearchSettings) => { settings.maxLastModFromString(s); },
            'minlastmod':
                (s: string, settings: SearchSettings) => { settings.minLastModFromString(s); },
            'out-dirpattern':
                (s: string, settings: SearchSettings) => { settings.addOutDirPatterns(s); },
            'out-archiveext':
                (s: string, settings: SearchSettings) => { settings.addOutArchiveExtensions(s); },
            'out-archivefilepattern':
                (s: string, settings: SearchSettings) => { settings.addOutArchiveFilePatterns(s); },
            'out-ext':
                (s: string, settings: SearchSettings) => { settings.addOutExtensions(s); },
            'out-filepattern':
                (s: string, settings: SearchSettings) => { settings.addOutFilePatterns(s); },
            'out-filetype':
                (s: string, settings: SearchSettings) => { settings.addOutFileTypes(s); },
            'out-linesafterpattern':
                (s: string, settings: SearchSettings) => { settings.addOutLinesAfterPatterns(s); },
            'out-linesbeforepattern':
                (s: string, settings: SearchSettings) => { settings.addOutLinesBeforePatterns(s); },
            'path':
                (s: string, settings: SearchSettings) => { settings.paths.push(s); },
            'searchpattern':
                (s: string, settings: SearchSettings) => { settings.addSearchPatterns(s); },
            'sort-by':
                (s: string, settings: SearchSettings) => { settings.sortBy = SortUtil.nameToSortBy(s); }
        };

        this.intActionMap = {
            'linesafter':
                (i: number, settings: SearchSettings) => { settings.linesAfter = i; },
            'linesbefore':
                (i: number, settings: SearchSettings) => { settings.linesBefore = i; },
            'maxdepth':
                (i: number, settings: SearchSettings) => { settings.maxDepth = i; },
            'maxlinelength':
                (i: number, settings: SearchSettings) => { settings.maxLineLength = i; },
            'maxsize':
                (i: number, settings: SearchSettings) => { settings.maxSize = i; },
            'mindepth':
                (i: number, settings: SearchSettings) => { settings.minDepth = i; },
            'minsize':
                (i: number, settings: SearchSettings) => { settings.minSize = i; },
        };

        this.setOptionsFromJsonFile();
    }

    // private static optCmp(o1: SearchOption, o2: SearchOption) {
    //     const a: string = o1.sortArg;
    //     const b: string = o2.sortArg;
    //     return a.localeCompare(b);
    // }

    // setOptionsFromJsonFile
    private setOptionsFromJsonFile(): void {
        const json = FileUtil.getFileContentsSync(config.SEARCH_OPTIONS_JSON_PATH);
        const obj = JSON.parse(json);
        if (Object.prototype.hasOwnProperty.call(obj, 'searchoptions') && Array.isArray(obj['searchoptions'])) {
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
            });
        } else throw new Error(`Invalid searchoptions file: ${config.SEARCH_OPTIONS_JSON_PATH}`);
        // this.options.sort(SearchOptions.optCmp);
    }

    public updateSettingsFromJson(json: string, settings: SearchSettings): Error | undefined {
        let err: Error | undefined = undefined;
        const obj = JSON.parse(json);
        // keys are sorted so that output is consistent across all versions
        const keys = Object.keys(obj).sort();
        const invalidKeys = keys.filter(k => !Object.prototype.hasOwnProperty.call(this.argNameMap, k));
        if (invalidKeys.length > 0) {
            return new SearchError(`Invalid option: ${invalidKeys[0]}`);
        }
        for (const k of keys) {
            if (err) break;
            if (obj[k] === undefined || obj[k] === null) {
                err = new SearchError(`Missing value for option ${k}`);
            }
            let longArg = this.argNameMap[k];
            if (this.boolActionMap[longArg]) {
                if (typeof obj[k] === 'boolean') {
                    this.boolActionMap[longArg](obj[k], settings);
                } else {
                    err = new SearchError(`Invalid value for option: ${k}`);
                }
            } else if (this.stringActionMap[longArg]) {
                if (typeof obj[k] === 'string') {
                    this.stringActionMap[longArg](obj[k], settings);
                } else if (typeof obj[k] === 'object' && obj[k].constructor === Array) {
                    obj[k].forEach((s: any) => {
                        if (typeof s === 'string') {
                            this.stringActionMap[longArg](s, settings);
                        } else {
                            err = new SearchError(`Invalid value for option: ${k}`);
                        }
                    });
                } else {
                    err = new SearchError(`Invalid value for option: ${k}`);
                }
            } else if (this.intActionMap[longArg]) {
                if (typeof obj[k] === 'number') {
                    this.intActionMap[longArg](obj[k], settings);
                } else {
                    err = new SearchError(`Invalid value for option: ${k}`);
                }
            } else {
                // should never get here
                err = new SearchError(`Invalid option: ${k}`);
            }
        }
        return err;
    }

    private updateSettingsFromFile(filePath: string, settings: SearchSettings): Error | undefined {
        const expandedPath = FileUtil.expandPath(filePath);
        if (fs.existsSync(expandedPath)) {
            if (filePath.endsWith('.json')) {
                const json: string = FileUtil.getFileContentsSync(expandedPath, settings.textFileEncoding);
                try {
                    return this.updateSettingsFromJson(json, settings);
                } catch (e: any) {
                    return new SearchError(`Unable to parse JSON in settings file: ${filePath}`);
                }
            } else {
                return new SearchError(`Invalid settings file (must be JSON): ${filePath}`);
            }
        } else {
            return new SearchError(`Settings file not found: ${filePath}`);
        }
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
                if (this.boolActionMap[longArg]) {
                    this.boolActionMap[longArg](true, settings);
                } else if (this.stringActionMap[longArg] || this.intActionMap[longArg] || longArg === 'settings-file') {
                    if (args.length > 0) {
                        if (this.stringActionMap[longArg]) {
                            this.stringActionMap[longArg](args.shift(), settings);
                        } else if (this.intActionMap[longArg]) {
                            this.intActionMap[longArg](parseInt(args.shift()!, 10), settings);
                        } else {
                            err = this.updateSettingsFromFile(args.shift()!, settings);
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

    public usageWithCode(exitCode: number): void {
        console.log(this.getUsageString());
        process.exit(exitCode);
    }

    public usage(): void {
        this.usageWithCode(0);
    }
}
