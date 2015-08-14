/// <reference path="../typings/node/node.d.ts"/>
/// <reference path="fileutil.ts"/>
/// <reference path="searchoption.ts"/>
/// <reference path="searchsettings.ts"/>
/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

"use strict";

var config = require('./config.js');
var FileUtil = require('./fileutil.js').FileUtil;
var SearchOption = require('./searchoption.js').SearchOption;
var SearchSettings = require('./searchsettings.js').SearchSettings;

interface StringOptionMap {
    [key: string]: SearchOption
}

class SearchOptions {
    // the list of SearchOption objects (populated by setOptionsFromXml)
    options: SearchOption[];
    argMap: StringOptionMap;
    flagMap: StringOptionMap;
    argActionMap: {};
    flagActionMap: {};

    constructor() {
        this.options = [];
        this.argMap = {};
        this.flagMap = {};

        this.argActionMap = {
            'in-archiveext':
                function(x: string, settings: SearchSettings): void { settings.addInArchiveExtension(x); },
            'in-archivefilepattern':
                function(x: string, settings: SearchSettings): void { settings.addInArchiveFilePattern(x); },
            'in-dirpattern':
                function(x: string, settings: SearchSettings): void { settings.addInDirPattern(x); },
            'in-ext':
                function(x: string, settings: SearchSettings): void { settings.addInExtension(x); },
            'in-filepattern':
                function(x: string, settings: SearchSettings): void { settings.addInFilePattern(x); },
            'in-linesafterpattern':
                function(x: string, settings: SearchSettings): void { settings.addInLinesAfterPattern(x); },
            'in-linesbeforepattern':
                function(x: string, settings: SearchSettings): void { settings.addInLinesBeforePattern(x); },
            'linesafter':
                function(x: string, settings: SearchSettings): void { settings.linesAfter = parseInt(x); },
            'linesaftertopattern':
                function(x: string, settings: SearchSettings): void { settings.addLinesAfterToPattern(x); },
            'linesafteruntilpattern':
                function(x: string, settings: SearchSettings): void { settings.addLinesAfterUntilPattern(x); },
            'linesbefore':
                function(x: string, settings: SearchSettings): void { settings.linesBefore = parseInt(x); },
            'maxlinelength':
                function(x: string, settings: SearchSettings): void { settings.maxLineLength = parseInt(x); },
            'out-dirpattern':
                function(x: string, settings: SearchSettings): void { settings.addOutDirPattern(x); },
            'out-archiveext':
                function(x: string, settings: SearchSettings): void { settings.addOutArchiveExtension(x); },
            'out-archivefilepattern':
                function(x: string, settings: SearchSettings): void { settings.addOutArchiveFilePattern(x); },
            'out-ext':
                function(x: string, settings: SearchSettings): void { settings.addOutExtension(x); },
            'out-filepattern':
                function(x: string, settings: SearchSettings): void { settings.addOutFilePattern(x); },
            'out-linesafterpattern':
                function(x: string, settings: SearchSettings): void { settings.addOutLinesAfterPattern(x); },
            'out-linesbeforepattern':
                function(x: string, settings: SearchSettings): void { settings.addOutLinesBeforePattern(x); },
            'search':
                function(x: string, settings: SearchSettings): void { settings.addSearchPattern(x); }
        };

        this.flagActionMap = {
            'allmatches':
                function(settings: SearchSettings): void { settings.firstMatch = false; },
            'archivesonly':
                function(settings: SearchSettings): void { settings.setArchivesOnly(); },
            'debug':
                function(settings: SearchSettings): void { settings.setDebug(); },
            'excludehidden':
                function(settings: SearchSettings): void { settings.excludeHidden = true; },
            'firstmatch':
                function(settings: SearchSettings): void { settings.firstMatch = true; },
            'includehidden':
                function(settings: SearchSettings): void { settings.excludeHidden = false; },
            'help':
                function(settings: SearchSettings): void { settings.printUsage = true; },
            'listdirs':
                function(settings: SearchSettings): void { settings.listDirs = true; },
            'listfiles':
                function(settings: SearchSettings): void { settings.listFiles = true; },
            'listlines':
                function(settings: SearchSettings): void { settings.listLines = true; },
            'multilinesearch':
                function(settings: SearchSettings): void { settings.multilineSearch = true; },
            'noprintmatches':
                function(settings: SearchSettings): void { settings.printResults = false; },
            'norecursive':
                function(settings: SearchSettings): void { settings.recursive = false; },
            'nosearcharchives':
                function(settings: SearchSettings): void { settings.searchArchives = false; },
            'printmatches':
                function(settings: SearchSettings): void { settings.printResults = true; },
            'recursive':
                function(settings: SearchSettings): void { settings.recursive = true; },
            'searcharchives':
                function(settings: SearchSettings): void { settings.searchArchives = true; },
            'uniquelines':
                function(settings: SearchSettings): void { settings.uniqueLines = true; },
            'verbose':
                function(settings: SearchSettings): void { settings.verbose = true; },
            'version':
                function(settings: SearchSettings): void { settings.printVersion = true; }
        };

        this.setOptionsFromXml();
    }

    private optcmp(o1: SearchOption, o2: SearchOption) {
        var a: string = o1.sortarg;
        var b: string = o2.sortarg;
        return a.localeCompare(b);
    }

    // setOptionsFromXml
    private setOptionsFromXml(): void {
        var me = this;
        var fs = require('fs');
        var DomJS = require('dom-js').DomJS;

        var domjs = new DomJS();
        var xml = fs.readFileSync(FileUtil.expandPath(config.SEARCHOPTIONSPATH)).toString();
        domjs.parse(xml, function(err: Error, dom) {
            if (err) {
                throw err;
            }
            for (var i: number = 0; i < dom.children.length; i++) {
                var child = dom.children[i];
                if (child.name && child.name === 'searchoption') {
                    var longArg: string = child.attributes.long;
                    var shortArg: string = child.attributes.short;
                    var desc: string = child.text().trim();
                    var func: any;
                    if (longArg in me.argActionMap) {
                        func = me.argActionMap[longArg];
                    }
                    else if (longArg in me.flagActionMap) {
                        func = me.flagActionMap[longArg];
                    }
                    else throw new Error("Unknown option: "+longArg);
                    var option = new SearchOption(shortArg, longArg, desc, func);
                    me.options.push(option);
                    if (longArg in me.argActionMap) {
                        me.argMap[longArg] = option;
                        if (shortArg) {
                            me.argMap[shortArg] = option;
                            me.argActionMap[shortArg] = me.argActionMap[longArg];
                        }
                    } else if (longArg in me.flagActionMap) {
                        me.flagMap[longArg] = option;
                        if (shortArg) {
                            me.flagMap[shortArg] = option;
                            me.flagActionMap[shortArg] = me.flagActionMap[longArg];
                        }
                    } else { // shouldn't get here
                        console.log("ERROR: " + longArg + " not found in either map");
                        me.usageWithCode(1);
                    }
                }
            }
        });
        this.options.sort(this.optcmp);
    }

    public settingsFromArgs(args: string[], callback) {
        var err: Error = null;
        var settings: SearchSettings = new SearchSettings();
        // default printResults to true since it's being run from cmd line
        settings.printResults = true;
        while(args) {
            var arg: string = args.shift();
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
                        this.argActionMap[arg](args.shift(), settings);
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
        callback(err, settings);
    }

    public usage(): void {
        this.usageWithCode(0);
    }

    public usageWithCode(exitCode: number): void {
        console.log(this.getUsageString());
        process.exit(exitCode);
    }

    private getUsageString(): string {
        var usage: string = 'Usage:\n tssearch [options] -s <searchpattern>' +
            ' <startpath>\n\nOptions:\n';
        var optStrings: string[] = [];
        var optDescs: string[] = [];
        var longest: number = 0;
        for (var i: number = 0; i < this.options.length; i++) {
            var opt: SearchOption = this.options[i];
            var optString: string = ' ';
            if (opt.shortarg)
                optString += '-' + opt.shortarg + ',';
            optString += '--' + opt.longarg;
            if (optString.length > longest)
                longest = optString.length;
            optStrings.push(optString);
            optDescs.push(opt.desc);
        }
        for (var i: number = 0; i < optStrings.length; i++) {
            var os: string = optStrings[i];
            while (os.length < longest)
                os += ' ';
            usage += os + '  ' + optDescs[i] + '\n';
        }
        return usage;
    }
}

exports.SearchOptions = SearchOptions;
