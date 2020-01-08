/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

const config = require('./config.js');
const FileTypes = require('./filetypes.js').FileTypes;
const FileUtil = require('./fileutil.js').FileUtil;
const SearchOption = require('./searchoption.js').SearchOption;
const SearchSettings = require('./searchsettings.js').SearchSettings;

function SearchOptions() {
    "use strict";
    let self = this;

    // the list of SearchOption objects (populated by setOptionsFromXml)
    let options = [];
    let argNameMap = {};
    let argMap = {};
    let flagMap = {};

    const argActionMap = {
        'encoding':
            (x, settings) => { settings.textFileEncoding = x; },
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
        'in-filetype':
            (x, settings) => { settings.addInFileType(x); },
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
        'out-filetype':
            (x, settings) => { settings.addOutFileType(x); },
        'out-linesafterpattern':
            (x, settings) => { settings.addOutLinesAfterPattern(x); },
        'out-linesbeforepattern':
            (x, settings) => { settings.addOutLinesBeforePattern(x); },
        'search':
            (x, settings) => { settings.addSearchPattern(x); },
        'settings-file':
            (x, settings) => { return settingsFromFile(x, settings); }

    };

    const boolFlagActionMap = {
        'allmatches':
            (b, settings) => { settings.firstMatch = !b; },
        'archivesonly':
            (b, settings) => { settings.setArchivesOnlyBool(b); },
        'debug':
            (b, settings) => { settings.setDebugBool(b); },
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
                    argNameMap[longArg] = longArg;
                    if (shortArg) argNameMap[shortArg] = longArg;
                    if (argActionMap[longArg]) func = argActionMap[longArg];
                    else if (boolFlagActionMap[longArg]) func = boolFlagActionMap[longArg];
                    else throw new Error("Unknown option: "+longArg);
                    const option = new SearchOption(shortArg, longArg, desc, func);
                    options.push(option);
                    if (argActionMap[longArg]) {
                        argMap[longArg] = option;
                        if (shortArg) argMap[shortArg] = option;
                    } else if (boolFlagActionMap[longArg]) {
                        flagMap[longArg] = option;
                        if (shortArg) flagMap[shortArg] = option;
                    }
                }
            });
        });
        options.sort(optcmp);
    })();

    var settingsFromFile = function (filepath, settings) {
        const fs = require('fs');
        if (fs.existsSync(filepath)) {
            let json = FileUtil.getFileContents(filepath);
            return settingsFromJson(json, settings);
        } else {
            return new Error('Settings file not found');
        }
    };

    self.settingsFromJson = function (json, settings) {
        let err = null;
        let obj = JSON.parse(json);
        for (let k in obj) {
            if (err) break;
            if (obj.hasOwnProperty(k)) {
                let longKey = argNameMap[k];
                if (argMap[k]) {
                    if (obj[k]) {
                        argMap[k].func(obj[k], settings);
                    } else {
                        err = new Error("Missing argument for option "+k);
                    }
                } else if (boolFlagActionMap[longKey]) {
                    boolFlagActionMap[longKey](obj[k], settings);
                } else if (k === 'startpath') {
                    settings.startPath = obj[k];
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
                        err = new Error("Missing argument for option " + arg);
                    }
                } else if (flagMap[arg]) {
                    flagMap[arg].func(true, settings);
                } else {
                    err = new Error("Invalid option: " + arg);
                }
            } else {
                settings.startPath = arg;
            }
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
