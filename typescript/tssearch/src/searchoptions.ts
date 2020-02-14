/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

"use strict";

import * as config from './config';
import {FileUtil} from './fileutil';
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
                (x: string, settings: SearchSettings) => { settings.addInArchiveExtension(x); },
            'in-archivefilepattern':
                (x: string, settings: SearchSettings) => { settings.addInArchiveFilePattern(x); },
            'in-dirpattern':
                (x: string, settings: SearchSettings) => { settings.addInDirPattern(x); },
            'in-ext':
                (x: string, settings: SearchSettings) => { settings.addInExtension(x); },
            'in-filepattern':
                (x: string, settings: SearchSettings) => { settings.addInFilePattern(x); },
            'in-filetype':
                (x: string, settings: SearchSettings) => { settings.addInFileType(x); },
            'in-linesafterpattern':
                (x: string, settings: SearchSettings) => { settings.addInLinesAfterPattern(x); },
            'in-linesbeforepattern':
                (x: string, settings: SearchSettings) => { settings.addInLinesBeforePattern(x); },
            'linesafter':
                (x: string, settings: SearchSettings) => { settings.linesAfter = parseInt(x); },
            'linesaftertopattern':
                (x: string, settings: SearchSettings) => { settings.addLinesAfterToPattern(x); },
            'linesafteruntilpattern':
                (x: string, settings: SearchSettings) => { settings.addLinesAfterUntilPattern(x); },
            'linesbefore':
                (x: string, settings: SearchSettings) => { settings.linesBefore = parseInt(x); },
            'maxlinelength':
                (x: string, settings: SearchSettings) => { settings.maxLineLength = parseInt(x); },
            'out-dirpattern':
                (x: string, settings: SearchSettings) => { settings.addOutDirPattern(x); },
            'out-archiveext':
                (x: string, settings: SearchSettings) => { settings.addOutArchiveExtension(x); },
            'out-archivefilepattern':
                (x: string, settings: SearchSettings) => { settings.addOutArchiveFilePattern(x); },
            'out-ext':
                (x: string, settings: SearchSettings) => { settings.addOutExtension(x); },
            'out-filepattern':
                (x: string, settings: SearchSettings) => { settings.addOutFilePattern(x); },
            'out-filetype':
                (x: string, settings: SearchSettings) => { settings.addOutFileType(x); },
            'out-linesafterpattern':
                (x: string, settings: SearchSettings) => { settings.addOutLinesAfterPattern(x); },
            'out-linesbeforepattern':
                (x: string, settings: SearchSettings) => { settings.addOutLinesBeforePattern(x); },
            'searchpattern':
                (x: string, settings: SearchSettings) => { settings.addSearchPattern(x); },
            'settings-file':
                (x: string, settings: SearchSettings) => { this.settingsFromFile(x, settings); }
        };

        this.boolFlagActionMap = {
            'allmatches':
                (b: boolean, settings: SearchSettings) => { settings.firstMatch = !b; },
            'archivesonly':
                (b: boolean, settings: SearchSettings) => { settings.setArchivesOnly(b); },
            'debug':
                (b: boolean, settings: SearchSettings) => { settings.setDebug(b); },
            'excludehidden':
                (b: boolean, settings: SearchSettings) => { settings.excludeHidden = b; },
            'firstmatch':
                (b: boolean, settings: SearchSettings) => { settings.firstMatch = b; },
            'includehidden':
                (b: boolean, settings: SearchSettings) => { settings.excludeHidden = !b; },
            'help':
                (b: boolean, settings: SearchSettings) => { settings.printUsage = b; },
            'listdirs':
                (b: boolean, settings: SearchSettings) => { settings.listDirs = b; },
            'listfiles':
                (b: boolean, settings: SearchSettings) => { settings.listFiles = b; },
            'listlines':
                (b: boolean, settings: SearchSettings) => { settings.listLines = b; },
            'multilinesearch':
                (b: boolean, settings: SearchSettings) => { settings.multilineSearch = b; },
            'noprintmatches':
                (b: boolean, settings: SearchSettings) => { settings.printResults = !b; },
            'norecursive':
                (b: boolean, settings: SearchSettings) => { settings.recursive = !b; },
            'nosearcharchives':
                (b: boolean, settings: SearchSettings) => { settings.searchArchives = !b; },
            'printmatches':
                (b: boolean, settings: SearchSettings) => { settings.printResults = b; },
            'recursive':
                (b: boolean, settings: SearchSettings) => { settings.recursive = b; },
            'searcharchives':
                (b: boolean, settings: SearchSettings) => { settings.searchArchives = b; },
            'uniquelines':
                (b: boolean, settings: SearchSettings) => { settings.uniqueLines = b; },
            'verbose':
                (b: boolean, settings: SearchSettings) => { settings.verbose = b; },
            'version':
                (b: boolean, settings: SearchSettings) => { settings.printVersion = b; }
        };

        this.setOptionsFromJsonFile();
    }

    private static optcmp(o1: SearchOption, o2: SearchOption) {
        const a: string = o1.sortarg;
        const b: string = o2.sortarg;
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

        let obj = JSON.parse(json);
        if (obj.hasOwnProperty('searchoptions') && Array.isArray(obj['searchoptions'])) {
            obj['searchoptions'].forEach(so => {
                let longArg = so['long'];
                let shortArg = '';
                if (so.hasOwnProperty('short'))
                    shortArg = so['short'];
                let desc = so['desc'];
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
        this.options.sort(SearchOptions.optcmp);
    }

    private settingsFromFile(filepath: string, settings: SearchSettings): Error | undefined {
        const fs = require('fs');
        if (fs.existsSync(filepath)) {
            let json: string = FileUtil.getFileContents(filepath, settings.textFileEncoding);
            return this.settingsFromJson(json, settings);
        } else {
            return new Error('Settings file not found');
        }
    }

    public settingsFromJson(json: string, settings: SearchSettings): Error | undefined {
        let err: Error | undefined = undefined;
        let obj = JSON.parse(json);
        for (let k in obj) {
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
                } else if (k == 'startpath') {
                    settings.startPath = obj[k];
                } else {
                    err = new Error("Invalid option: "+k);
                }
            }
        }
        return err;
    }

    public settingsFromArgs(args: string[], cb: (err: Error | undefined, settings: SearchSettings) => void) {
        let err: Error | undefined = undefined;
        let settings: SearchSettings = new SearchSettings();
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
                let longarg = this.argNameMap[arg];
                if (this.argMap[longarg]) {
                    if (args.length > 0) {
                        err = this.argActionMap[longarg](args.shift(), settings);
                    } else {
                        err = new Error("Missing argument for option " + arg);
                    }
                } else if (this.flagMap[longarg]) {
                    this.boolFlagActionMap[longarg](true, settings);
                } else {
                    err = new Error("Invalid option: " + arg);
                }
            } else {
                settings.startPath = arg;
            }
        }
        if (settings.debug) {
            settings.verbose = true;
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
            ' <startpath>\n\nOptions:\n';
        let optStrings: string[] = [];
        let optDescs: string[] = [];
        let longest: number = 0;
        this.options.forEach((opt: SearchOption) => {
            let optString: string = ' ';
            if (opt.shortarg)
                optString += '-' + opt.shortarg + ',';
            optString += '--' + opt.longarg;
            if (optString.length > longest)
                longest = optString.length;
            optStrings.push(optString);
            optDescs.push(opt.desc);
        });
        for (let i: number = 0; i < optStrings.length; i++) {
            let os: string = optStrings[i];
            while (os.length < longest)
                os += ' ';
            usage += os + '  ' + optDescs[i] + '\n';
        }
        return usage;
    }
}
