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
            function(x, settings) { settings.addInArchiveExtension(x); },
        'in-archivefilepattern':
            function(x, settings) { settings.addInArchiveFilePattern(x); },
        'in-dirpattern':
            function(x, settings) { settings.addInDirPattern(x); },
        'in-ext':
            function(x, settings) { settings.addInExtension(x); },
        'in-filepattern':
            function(x, settings) { settings.addInFilePattern(x); },
        'in-linesafterpattern':
            function(x, settings) { settings.addInLinesAfterPattern(x); },
        'in-linesbeforepattern':
            function(x, settings) { settings.addInLinesBeforePattern(x); },
        'linesafter':
            function(x, settings) { settings.linesAfter = parseInt(x); },
        'linesaftertopattern':
            function(x, settings) { settings.addLinesAfterToPattern(x); },
        'linesafteruntilpattern':
            function(x, settings) { settings.addLinesAfterUntilPattern(x); },
        'linesbefore':
            function(x, settings) { settings.linesBefore = parseInt(x); },
        'maxlinelength':
            function(x, settings) { settings.maxLineLength = parseInt(x); },
        'out-dirpattern':
            function(x, settings) { settings.addOutDirPattern(x); },
        'out-archiveext':
            function(x, settings) { settings.addOutArchiveExtension(x); },
        'out-archivefilepattern':
            function(x, settings) { settings.addOutArchiveFilePattern(x); },
        'out-ext':
            function(x, settings) { settings.addOutExtension(x); },
        'out-filepattern':
            function(x, settings) { settings.addOutFilePattern(x); },
        'out-linesafterpattern':
            function(x, settings) { settings.addOutLinesAfterPattern(x); },
        'out-linesbeforepattern':
            function(x, settings) { settings.addOutLinesBeforePattern(x); },
        'search':
            function(x, settings) { settings.addSearchPattern(x); }
    };

    const flagActionMap = {
        'allmatches':
            function(settings) { settings.firstMatch = false; },
        'archivesonly':
            function(settings) { settings.setArchivesOnly(); },
        'debug':
            function(settings) { settings.setDebug(); },
        'excludehidden':
            function(settings) { settings.excludeHidden = true; },
        'firstmatch':
            function(settings) { settings.firstMatch = true; },
        'includehidden':
            function(settings) { settings.excludeHidden = false; },
        'help':
            function(settings) { settings.printUsage = true; },
        'listdirs':
            function(settings) { settings.listDirs = true; },
        'listfiles':
            function(settings) { settings.listFiles = true; },
        'listlines':
            function(settings) { settings.listLines = true; },
        'multilinesearch':
            function(settings) { settings.multilineSearch = true; },
        'noprintmatches':
            function(settings) { settings.printResults = false; },
        'norecursive':
            function(settings) { settings.recursive = false; },
        'nosearcharchives':
            function(settings) { settings.searchArchives = false; },
        'printmatches':
            function(settings) { settings.printResults = true; },
        'recursive':
            function(settings) { settings.recursive = true; },
        'searcharchives':
            function(settings) { settings.searchArchives = true; },
        'uniquelines':
            function(settings) { settings.uniqueLines = true; },
        'verbose':
            function(settings) { settings.verbose = true; },
        'version':
            function(settings) { settings.printVersion = true; }
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

    self.settingsFromArgs = function (args, cb) {
        let err = null;
        let settings = new SearchSettings();
        // default printResults to true since it's being run from cmd line
        settings.printResults = true;
        while(args) {
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
                        argMap[arg].func(args.shift(), settings);
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
