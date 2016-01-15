/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

var config = require('./config.js');
var FileUtil = require('./fileutil.js').FileUtil;
var SearchOption = require('./searchoption.js').SearchOption;
var SearchSettings = require('./searchsettings.js').SearchSettings;

function SearchOptions() {
    "use strict";
    let self = this;

    // the list of SearchOption objects (populated by setOptionsFromXml)
    let options = [];
    let argMap = {};
    let flagMap = {};

    const argActionMap = {
        'in-archiveext':
            (x, settings) => { settings.addInArchiveExtension(x); },
        'in-archivefilepattern':
            (x, settings) => { settings.addInArchiveFilePattern(x); },
        'in-dirpattern':
            (x, settings) => { settings.addInDirPattern(x); },
        'in-ext':
            (x, settings) => { settings.addInExtension(x); },
        'in-filepattern':
            (x, settings) => { settings.addInFilePattern(x); },
        'in-linesafterpattern':
            (x, settings) => { settings.addInLinesAfterPattern(x); },
        'in-linesbeforepattern':
            (x, settings) => { settings.addInLinesBeforePattern(x); },
        'linesafter':
            (x, settings) => { settings.linesAfter = parseInt(x); },
        'linesaftertopattern':
            (x, settings) => { settings.addLinesAfterToPattern(x); },
        'linesafteruntilpattern':
            (x, settings) => { settings.addLinesAfterUntilPattern(x); },
        'linesbefore':
            (x, settings) => { settings.linesBefore = parseInt(x); },
        'maxlinelength':
            (x, settings) => { settings.maxLineLength = parseInt(x); },
        'out-dirpattern':
            (x, settings) => { settings.addOutDirPattern(x); },
        'out-archiveext':
            (x, settings) => { settings.addOutArchiveExtension(x); },
        'out-archivefilepattern':
            (x, settings) => { settings.addOutArchiveFilePattern(x); },
        'out-ext':
            (x, settings) => { settings.addOutExtension(x); },
        'out-filepattern':
            (x, settings) => { settings.addOutFilePattern(x); },
        'out-linesafterpattern':
            (x, settings) => { settings.addOutLinesAfterPattern(x); },
        'out-linesbeforepattern':
            (x, settings) => { settings.addOutLinesBeforePattern(x); },
        'search':
            (x, settings) => { settings.addSearchPattern(x); },
        'settings-file':
             (x, settings) => { return settingsFromFile(x, settings); }

    };

    const flagActionMap = {
        'allmatches':
            settings => { settings.firstMatch = false; },
        'archivesonly':
            settings => { settings.setArchivesOnly(); },
        'debug':
            settings => { settings.setDebug(); },
        'excludehidden':
            settings => { settings.excludeHidden = true; },
        'firstmatch':
            settings => { settings.firstMatch = true; },
        'includehidden':
            settings => { settings.excludeHidden = false; },
        'help':
            settings => { settings.printUsage = true; },
        'listdirs':
            settings => { settings.listDirs = true; },
        'listfiles':
            settings => { settings.listFiles = true; },
        'listlines':
            settings => { settings.listLines = true; },
        'multilinesearch':
            settings => { settings.multilineSearch = true; },
        'noprintmatches':
            settings => { settings.printResults = false; },
        'norecursive':
            settings => { settings.recursive = false; },
        'nosearcharchives':
            settings => { settings.searchArchives = false; },
        'printmatches':
            settings => { settings.printResults = true; },
        'recursive':
            settings => { settings.recursive = true; },
        'searcharchives':
            settings => { settings.searchArchives = true; },
        'uniquelines':
            settings => { settings.uniqueLines = true; },
        'verbose':
            settings => { settings.verbose = true; },
        'version':
            settings => { settings.printVersion = true; }
    };

    function optcmp(o1, o2) {
        const a = o1.sortarg;
        const b = o2.sortarg;
        return a.localeCompare(b);
    }

    // setOptionsFromXml
    (function () {
        const fs = require('fs');
        const DomJS = require('dom-js').DomJS;

        const domjs = new DomJS();
        const xml = fs.readFileSync(FileUtil.expandPath(config.SEARCHOPTIONSPATH)).toString();
        domjs.parse(xml, function(err, dom) {
            if (err) {
                throw err;
            }
            dom.children.forEach(child => {
                if (child.name && child.name === 'searchoption') {
                    const longArg = child.attributes.long;
                    const shortArg = child.attributes.short;
                    const desc = child.text().trim();
                    let func = null;
                    if (argActionMap[longArg]) func = argActionMap[longArg];
                    else if (flagActionMap[longArg]) func = flagActionMap[longArg];
                    else throw new Error("Unknown option: "+longArg);
                    const option = new SearchOption(shortArg, longArg, desc, func);
                    options.push(option);
                    if (argActionMap[longArg]) {
                        argMap[longArg] = option;
                        if (shortArg) argMap[shortArg] = option;
                    } else if (flagActionMap[longArg]) {
                        flagMap[longArg] = option;
                        if (shortArg) flagMap[shortArg] = option;
                    }
                }
            });
        });
        options.sort(optcmp);
    })();

    var settingsFromFile = function (filepath, settings) {
        let err = null;
        let fileSettings = {};
        const fs = require('fs');
        if (fs.existsSync(filepath)) {
            fileSettings = require(filepath);
        } else {
            return new Error('Settings file not found');
        }
        for (let k in fileSettings) {
            if (err) break;
            if (fileSettings.hasOwnProperty(k)) {
                if (argMap[k]) {
                    if (fileSettings[k]) {
                        argMap[k].func(fileSettings[k], settings);
                    } else {
                        err = new Error("Missing argument for option "+k);
                    }
                } else if (flagMap[k]) {
                    flagMap[k].func(settings);
                } else if (k == 'startpath') {
                    settings.startPath = fileSettings[k];
                } else {
                    err = new Error("Invalid option: "+k);
                }
            }
        }
        return err;
    };

    self.settingsFromArgs = function (args, cb) {
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
                if (argMap[arg]) {
                    if (args.length > 0) {
                        err = argMap[arg].func(args.shift(), settings);
                    } else {
                        err = new Error("Missing argument for option "+arg);
                    }
                } else if (flagMap[arg]) {
                    flagMap[arg].func(settings);
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
    };

    self.usage = function () {
        self.usageWithCode(0);
    };

    self.usageWithCode = function (exitCode) {
        console.log(getUsageString());
        process.exit(exitCode);
    };

    const getUsageString = function () {
        let usage = 'Usage:\n jssearch [options] -s <searchpattern> <startpath>\n\n';
        usage += 'Options:\n';
        let optStrings = [];
        let optDescs = [];
        let longest = 0;
        options.forEach(opt => {
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
    };
}

exports.SearchOptions = SearchOptions;
