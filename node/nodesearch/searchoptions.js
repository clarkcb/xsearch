/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

var FileUtil = require('./fileutil.js').FileUtil;
var SearchOption = require('./searchoption.js').SearchOption;
var SearchSettings = require('./searchsettings.js').SearchSettings;

function SearchOptions() {
    var that = this;

    //TODO: move to config file
    var searchOptionsPath = '~/src/git/xsearch/shared/searchoptions.xml';

    // the list of SearchOption objects (populated by setOptionsFromXml)
    var options = [];
    var argMap = {};
    var flagMap = {};

    var argActionMap = {
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
            function(x, settings) { settings.linesAfter = x; },
        'linesaftertopattern':
            function(x, settings) { settings.addInLinesAfterToPattern(x); },
        'linesafteruntilpattern':
            function(x, settings) { settings.addInLinesAfterUntilPattern(x); },
        'linesbefore':
            function(x, settings) { settings.linesBefore = x; },
        'maxlinelength':
            function(x, settings) { settings.maxLineLength = x; },
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

    var flagActionMap = {
        'allmatches':
            function(settings) { settings.firstMatch = false; },
        'archivesonly':
            function(settings) { settings.archivesOnly = true;
                                 settings.searchArchives = true; },
        'debug':
            function(settings) { settings.debug = true; },
        'dotiming':
            function(settings) { settings.doTiming = true; },
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
        var a = o1.sortarg;
        var b = o2.sortarg;
        return a.localeCompare(b);
    }

    // setOptionsFromXml
    (function () {
        var fs = require('fs');
        var DomJS = require('dom-js').DomJS;

        var domjs = new DomJS();

        var xml = fs.readFileSync(FileUtil.expandPath(searchOptionsPath)).toString();
        domjs.parse(xml, function(err, dom) {
            if (err) {
                throw err;
            }
            for (var i in dom.children) {
                var child = dom.children[i];
                if (child.name && child.name === 'searchoption') {
                    var longArg = child.attributes.long;
                    var shortArg = child.attributes.short;
                    var desc = child.text().trim();
                    var func = null;
                    if (argActionMap[longArg]) func = argActionMap[longArg];
                    else if (flagActionMap[longArg]) func = flagActionMap[longArg];
                    else throw new Error("Unknown option: "+longArg);
                    var option = new SearchOption(shortArg, longArg, desc, func);
                    options.push(option);
                    if (argActionMap[longArg]) {
                        argMap[longArg] = option;
                        if (shortArg) argMap[shortArg] = option;
                    } else if (flagActionMap[longArg]) {
                        flagMap[longArg] = option;
                        if (shortArg) flagMap[shortArg] = option;
                    }
                }
            }
        });
        options.sort(optcmp);
    })();

    this.searchSettingsFromArgs = function (args, callback) {
        var err = null;
        var settings = new SearchSettings();
        // default printResults to true since it's being run from cmd line
        settings.printResults = true;
        while(args) {
            var arg = args.shift();
            if (!arg) {
                break;
            }
            if (arg.charAt(0) === '-') {
                while (arg && arg.charAt(0) === '-') {
                    arg = arg.substring(1);
                }
                if (argMap[arg]) {
                    if (args) {
                        argMap[arg].func(args.shift(), settings);
                    } else {
                        err = new Error("Missing argument for option "+arg);
                    }
                } else if (flagMap[arg]) {
                    flagMap[arg].func(settings);
                    if (['h','help','V','version'].indexOf(arg) > -1)
                        return settings;
                } else {
                    err = new Error("Unknown option: "+arg);
                }
            } else {
                settings.startPath = arg;
            }
        }
        if (settings.debug) {
            settings.verbose = true;
        }
        callback(err, settings);
    };

    this.usage = function () {
        usageWithCode(0);
    };

    this.usageWithCode = function (exitCode) {
        console.log(getUsageString());
        process.exit(exitCode);
    };

    var getUsageString = function () {
        var usage = 'Usage:\nnodesearch [options] <startpath>\n\n';
        usage += 'Options:\n';
        var optStrings = [];
        var optDescs = [];
        var longest = 0;
        for (var o in options) {
            var opt = options[o];
            var optString = '';
            if (opt.shortarg)
                optString += '-' + opt.shortarg + ',';
            optString += '--' + opt.longarg;
            if (optString.length > longest)
                longest = optString.length;
            optStrings.push(optString);
            optDescs.push(opt.desc)
        }
        for (var o in optStrings) {
            var optString = optStrings[o];
            while (optString.length < longest)
                optString += ' ';
            usage += optString + '  ' + optDescs[o] + '\n';
        }
        return usage;
    };
}

exports.SearchOptions = SearchOptions;
