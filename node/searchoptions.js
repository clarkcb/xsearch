/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

var SearchOption = require('./searchoption.js').SearchOption;
var SearchSettings = require('./searchsettings.js').SearchSettings;

function SearchOptions() {
    var that = this;

    //TODO: move to config file
    var searchOptionsPath = '/Users/cary/src/git/xsearch/shared/searchoptions.xml';

    // the list of SearchOption objects (populated by setOptionsFromXml)
    var options = [];
    var argMap = {};
    var flagMap = {};

    var argActionMap = {
        'in-dirpattern':
            function(x, settings) { settings.addInDirnamePattern(x); },
        'in-ext':
            function(x, settings) { settings.addInExtension(x); },
        'in-filepattern':
            function(x, settings) { settings.addInFilenamePattern(x); },
        'in-linesafterpattern':
            function(x, settings) { settings.addInLinesAfterPattern(x); },
        'in-linesbeforepattern':
            function(x, settings) { settings.addInLinesBeforePattern(x); },
        'linesbefore':
            function(x, settings) { settings.numLinesBefore = x; },
        'linesafter':
            function(x, settings) { settings.numLinesAfter = x; },
        'out-dirpattern':
            function(x, settings) { settings.addOutDirnamePattern(x); },
        'out-ext':
            function(x, settings) { settings.addOutExtension(x); },
        'out-filepattern':
            function(x, settings) { settings.addOutFilenamePattern(x); },
        'out-linesafterpattern':
            function(x, settings) { settings.addOutLinesAfterPattern(x); },
        'out-linesbeforepattern':
            function(x, settings) { settings.addOutLinesBeforePattern(x); },
        'search':
            function(x, settings) { settings.addSearchPattern(x); },
    };

    var flagActionMap = {
        'allmatches':
            function(settings) { settings.firstMatch = false; },
        'debug':
            function(settings) { settings.debug = true; },
        'dotiming':
            function(settings) { settings.doTiming = true; },
        'firstmatch':
            function(settings) { settings.firstMatch = true; },
        'help':
            function(settings) { settings.printUsage = true; },
        'listfiles':
            function(settings) { settings.listFiles = true; },
        'listlines':
            function(settings) { settings.listLines = true; },
        'multilinesearch':
            function(settings) { settings.multilineSearch = true; },
        'noprintmatches':
            function(settings) { settings.printResults = false; },
        'nosearchcompressed':
            function(settings) { settings.searchCompressed = false; },
        'printmatches':
            function(settings) { settings.printResults = true; },
        'searchcompressed':
            function(settings) { settings.searchCompressed = true; },
        'verbose':
            function(settings) { settings.verbose = true; },
        'version':
            function(settings) { settings.printVersion = true; },
    };

    function optcmp(o1, o2) {
        var a = o1.sortarg;
        var b = o2.sortarg;
        return a.localeCompare(b);
    }

    var setOptionsFromXml = function () {
        var fs = require('fs');
        var DomJS = require('dom-js').DomJS;

        var domjs = new DomJS();

        var xml = fs.readFileSync(searchOptionsPath).toString();
        domjs.parse(xml, function(err, dom) {
            for (var i in dom.children) {
                var child = dom.children[i];
                if (child.name && child.name === 'searchoption') {
                    var long = child.attributes.long;
                    var short = child.attributes.short;
                    var desc = child.text().trim();
                    var func = null;
                    if (argActionMap[long]) func = argActionMap[long];
                    else if (flagActionMap[long]) func = flagActionMap[long];
                    else throw new Error("Unknown option: "+long);
                    var option = new SearchOption(short, long, desc, func);
                    options.push(option);
                    if (argActionMap[long]) {
                        argMap[long] = option;
                        if (short) argMap[short] = option;
                    } else if (flagActionMap[long]) {
                        flagMap[long] = option;
                        if (short) flagMap[short] = option;
                    }
                }
            }
        });
        options.sort(optcmp);
    };
    setOptionsFromXml();

    this.searchSettingsFromArgs = function (args) {
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
                        throw new Error("Missing argument for option "+arg);
                    }
                } else if (flagMap[arg]) {
                    flagMap[arg].func(settings);
                    if (['h','help','V','version'].indexOf(arg) > -1)
                        return settings;
                } else {
                    throw new Error("Unknown option: "+arg);
                }
            } else {
                settings.startPath = arg;
            }
        }
        return settings;
    };

    this.usage = function () {
        usageWithCode(0);
    }

    this.usageWithCode = function (exitCode) {
        console.log(getUsageString());
        process.exit(exitCode);
    }

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
                optString += '-' + opt.shortarg + ','
            optString += '--' + opt.longarg
            if (optString.length > longest)
                longest = optString.length
            optStrings.push(optString)
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


};

exports.SearchOptions = SearchOptions;
