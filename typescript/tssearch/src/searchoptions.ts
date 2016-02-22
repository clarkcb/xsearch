/// <reference path="../typings/node/node.d.ts"/>
/// <reference path="searchoption.ts"/>
/// <reference path="searchsettings.ts"/>
/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

"use strict";

var config = require('./config.ts');
var FileUtil = require('./fileutil.ts').FileUtil;
var SearchOption = require('./searchoption.ts').SearchOption;
var SearchSettings = require('./searchsettings.ts').SearchSettings;

interface StringOptionMap {
    [key: string]: SearchOption
}

interface StringActionMap {
    [key: string]: any
}

class SearchOptions {
    // the list of SearchOption objects (populated by setOptionsFromXml)
    options: SearchOption[];
    argMap: StringOptionMap;
    flagMap: StringOptionMap;
    argActionMap: StringActionMap;
    boolFlagActionMap: StringActionMap;
    flagActionMap: StringActionMap;

    constructor() {
        this.options = [];
        this.argMap = {};
        this.flagMap = {};

        this.argActionMap = {
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
            'out-linesafterpattern':
                (x: string, settings: SearchSettings) => { settings.addOutLinesAfterPattern(x); },
            'out-linesbeforepattern':
                (x: string, settings: SearchSettings) => { settings.addOutLinesBeforePattern(x); },
            'search':
                (x: string, settings: SearchSettings) => { settings.addSearchPattern(x); },
            'settings-file':
                (x: string, settings: SearchSettings) => { this.settingsFromFile(x, settings); }
        };

        this.boolFlagActionMap = {
            'allmatches':
                (b: boolean, settings: SearchSettings) => { settings.firstMatch = !b; },
            'archivesonly':
                (b: boolean, settings: SearchSettings) => { settings.setArchivesOnlyBool(b); },
            'debug':
                (b: boolean, settings: SearchSettings) => { settings.setDebugBool(b); },
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


        this.flagActionMap = {
            'allmatches':
                (settings: SearchSettings) => { settings.firstMatch = false; },
            'archivesonly':
                (settings: SearchSettings) => { settings.setArchivesOnly(); },
            'debug':
                (settings: SearchSettings) => { settings.setDebug(); },
            'excludehidden':
                (settings: SearchSettings) => { settings.excludeHidden = true; },
            'firstmatch':
                (settings: SearchSettings) => { settings.firstMatch = true; },
            'includehidden':
                (settings: SearchSettings) => { settings.excludeHidden = false; },
            'help':
                (settings: SearchSettings) => { settings.printUsage = true; },
            'listdirs':
                (settings: SearchSettings) => { settings.listDirs = true; },
            'listfiles':
                (settings: SearchSettings) => { settings.listFiles = true; },
            'listlines':
                (settings: SearchSettings) => { settings.listLines = true; },
            'multilinesearch':
                (settings: SearchSettings) => { settings.multilineSearch = true; },
            'noprintmatches':
                (settings: SearchSettings) => { settings.printResults = false; },
            'norecursive':
                (settings: SearchSettings) => { settings.recursive = false; },
            'nosearcharchives':
                (settings: SearchSettings) => { settings.searchArchives = false; },
            'printmatches':
                (settings: SearchSettings) => { settings.printResults = true; },
            'recursive':
                (settings: SearchSettings) => { settings.recursive = true; },
            'searcharchives':
                (settings: SearchSettings) => { settings.searchArchives = true; },
            'uniquelines':
                (settings: SearchSettings) => { settings.uniqueLines = true; },
            'verbose':
                (settings: SearchSettings) => { settings.verbose = true; },
            'version':
                (settings: SearchSettings) => { settings.printVersion = true; }
        };

        this.setOptionsFromXml();
    }

    private optcmp(o1: SearchOption, o2: SearchOption) {
        const a: string = o1.sortarg;
        const b: string = o2.sortarg;
        return a.localeCompare(b);
    }

    // setOptionsFromXml
    private setOptionsFromXml(): void {
        const self = this;
        const fs = require('fs');
        const DomJS = require('dom-js').DomJS;

        const domjs = new DomJS();
        const xml = fs.readFileSync(FileUtil.expandPath(config.SEARCHOPTIONSPATH)).toString();
        domjs.parse(xml, function(err: Error, dom) {
            if (err) {
                throw err;
            }
            dom.children.forEach(child => {
                if (child.name && child.name === 'searchoption') {
                    const longArg: string = child.attributes.long;
                    const shortArg: string = child.attributes.short;
                    const desc: string = child.text().trim();
                    let func: any;
                    if (longArg in self.argActionMap) {
                        func = self.argActionMap[longArg];
                    }
                    else if (longArg in self.flagActionMap) {
                        func = self.flagActionMap[longArg];
                    }
                    else throw new Error("Unknown option: "+longArg);
                    const option = new SearchOption(shortArg, longArg, desc, func);
                    self.options.push(option);
                    if (longArg in self.argActionMap) {
                        self.argMap[longArg] = option;
                        if (shortArg) {
                            self.argMap[shortArg] = option;
                            self.argActionMap[shortArg] = self.argActionMap[longArg];
                        }
                    } else if (longArg in self.flagActionMap) {
                        self.flagMap[longArg] = option;
                        if (shortArg) {
                            self.flagMap[shortArg] = option;
                            self.flagActionMap[shortArg] = self.flagActionMap[longArg];
                        }
                    } else { // shouldn't get here
                        console.log("ERROR: " + longArg + " not found in either map");
                        self.usageWithCode(1);
                    }
                }
            });
        });
        this.options.sort(this.optcmp);
    }

    private settingsFromFile(filepath: string, settings: SearchSettings): Error {
        const fs = require('fs');
        if (fs.existsSync(filepath)) {
            let json: string = FileUtil.getFileContents(filepath);
            return this.settingsFromJson(json, settings);
        } else {
            return new Error('Settings file not found');
        }
    }

    public settingsFromJson(json: string, settings: SearchSettings): Error {
        let err: Error = null;
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

    public settingsFromArgs(args: string[], cb) {
        let err: Error = null;
        let settings: SearchSettings = new SearchSettings();
        // default printResults to true since it's being run from cmd line
        settings.printResults = true;
        while(args && !err) {
            let arg: string = args.shift();
            if (!arg) {
                break;
            }
            if (arg.charAt(0) === '-') {
                while (arg && arg.charAt(0) === '-') {
                    arg = arg.substring(1);
                }
                if (this.argMap[arg]) {
                    if (args.length > 0) {
                        //this.argMap[arg].func(args.shift(), settings);
                        err = this.argActionMap[arg](args.shift(), settings);
                    } else {
                        err = new Error("Missing argument for option "+arg);
                    }
                } else if (this.flagMap[arg]) {
                    //this.flagMap[arg].func(settings);
                    this.flagActionMap[arg](settings);
                } else {
                    err = new Error("Invalid option: "+arg);
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

exports.SearchOptions = SearchOptions;
