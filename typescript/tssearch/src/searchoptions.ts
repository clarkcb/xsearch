/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

'use strict';

import * as fs from 'fs';

import * as config from './config';
import {ArgToken, ArgTokenizer, ArgTokenType, FileUtil, SortUtil} from 'tsfind';
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
    argTokenizer: ArgTokenizer;

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
                (b: boolean, settings: SearchSettings) => { settings.printMatches = !b; },
            'noprintresults':
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
                (b: boolean, settings: SearchSettings) => { settings.printMatches = b; },
            'printresults':
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
        this.argTokenizer = new ArgTokenizer(this.options);
    }

    // setOptionsFromJsonFile
    private setOptionsFromJsonFile(): void {
        const json = FileUtil.getFileContentsSync(config.SEARCH_OPTIONS_JSON_PATH);
        const obj = JSON.parse(json);
        if (Object.prototype.hasOwnProperty.call(obj, 'searchoptions') && Array.isArray(obj['searchoptions'])) {
            obj['searchoptions'].forEach(so => {
                const longArg = so['long'];
                let shortArg = '';
                if (Object.prototype.hasOwnProperty.call(so, 'short')) {
                    shortArg = so['short'];
                }
                const desc = so['desc'];
                if (shortArg) this.argNameMap[shortArg] = longArg;
                let argType = ArgTokenType.Unknown;
                if (this.boolActionMap[longArg]) {
                    argType = ArgTokenType.Bool;
                } else if (this.stringActionMap[longArg]) {
                    argType = ArgTokenType.Str;
                } else if (this.intActionMap[longArg]) {
                    argType = ArgTokenType.Int;
                }
                this.options.push(new SearchOption(shortArg, longArg, desc, argType));
            });
        } else throw new Error(`Invalid searchoptions file: ${config.SEARCH_OPTIONS_JSON_PATH}`);
    }

    private updateSettingsFromArgTokens(settings: SearchSettings, argTokens: ArgToken[]): Error | undefined {
        let err: Error | undefined = undefined;
        for (const argToken of argTokens) {
            if (err) break;
            if (argToken.type === ArgTokenType.Bool) {
                if (typeof argToken.value === 'boolean') {
                    this.boolActionMap[argToken.name](argToken.value, settings);
                } else {
                    err = new SearchError(`Invalid value for option: ${argToken}`);
                }
            } else if (argToken.type === ArgTokenType.Str) {
                if (argToken.name === 'settings-file') {
                    err = this.updateSettingsFromFile(settings, argToken.value);
                } else if (typeof argToken.value === 'string') {
                    this.stringActionMap[argToken.name](argToken.value, settings);
                } else if (typeof argToken.value === 'object' && Array.isArray(argToken.value)) {
                    argToken.value.forEach(s => {
                        if (typeof s === 'string') {
                            this.stringActionMap[argToken.name](s, settings);
                        } else {
                            err = new SearchError(`Invalid value for option: ${argToken}`);
                        }
                    });
                } else {
                    err = new SearchError(`Invalid value for option: ${argToken}`);
                }
            } else if (argToken.type === ArgTokenType.Int) {
                if (typeof argToken.value === 'number') {
                    this.intActionMap[argToken.name](argToken.value, settings);
                } else {
                    err = new SearchError(`Invalid value for option: ${argToken}`);
                }
            } else {
                err = new SearchError(`Invalid option: ${argToken}`);
            }
        }
        return err;
    }

    public updateSettingsFromJson(settings: SearchSettings, json: string): Error | undefined {
        let { err, argTokens } = this.argTokenizer.tokenizeJson(json);
        if (!err) {
            err = this.updateSettingsFromArgTokens(settings, argTokens);
        }
        return err;
    }

    public updateSettingsFromFile(settings: SearchSettings, filePath: string): Error | undefined {
        let { err, argTokens } = this.argTokenizer.tokenizeFile(filePath);
        if (!err) {
            err = this.updateSettingsFromArgTokens(settings, argTokens);
        }
        return err;
    }

    public updateSettingsFromArgs(settings: SearchSettings, args: string[]): Error | undefined {
        let { err, argTokens } = this.argTokenizer.tokenizeArgs(args);
        if (!err) {
            err = this.updateSettingsFromArgTokens(settings, argTokens);
        }
        return err;
    }

    public settingsFromArgs(args: string[], cb: (err: Error | undefined, settings: SearchSettings) => void): void {
        const settings: SearchSettings = new SearchSettings();
        // default printResults to true since it's being run from cmd line
        settings.printResults = true;
        let err = this.updateSettingsFromArgs(settings, args);
        cb(err, settings);
    }

    private static optCmp(o1: SearchOption, o2: SearchOption) {
        const a: string = o1.sortArg;
        const b: string = o2.sortArg;
        return a.localeCompare(b);
    }

    private getUsageString(): string {
        let usage: string = 'Usage:\n tssearch [options] -s <searchpattern>' +
            ' <path> [<path> ...]\n\nOptions:\n';
        const optStrings: string[] = [];
        const optDescs: string[] = [];
        let longest = 0;
        this.options.sort(SearchOptions.optCmp);
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
