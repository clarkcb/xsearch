/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

var SearchOption = require('./searchoption.js').SearchOption;
var SearchSettings = require('./searchsettings.js').SearchSettings;

function SearchOptions() {
    var that = this;

    var argOptions = [
        new SearchOption('b', 'linesbefore',
            'Number of lines to show before every matched line (default: 0)',
            function(x, settings) { settings.numLinesBefore = x; }),
        new SearchOption('B', 'linesafter',
            'Number of lines to show after every matched line (default: 0)',
            function(x, settings) { settings.numLinesAfter = x; }),
        new SearchOption('d', 'in-dirpattern',
            'Specify name pattern for directories to include in search',
            function(x, settings) { settings.addInDirnamePattern(x); }),
        new SearchOption('D', 'out-dirpattern',
            'Specify name pattern for directories to exclude from search',
            function(x, settings) { settings.addOutDirnamePattern(x); }),
        new SearchOption('f', 'in-filepattern',
            'Specify name pattern for files to include in search',
            function(x, settings) { settings.addInFilenamePattern(x); }),
        new SearchOption('F', 'out-filepattern',
            'Specify name pattern for files to exclude from search',
            function(x, settings) { settings.addOutFilenamePattern(x); }),
        //new SearchOption('', 'out-linesafterpattern',
        //    'Specify pattern to filter the "lines-after" lines on (used with --linesafter)',
        //    function(x, settings) { settings.addOutLinesAfterPattern(x); }),
        //new SearchOption('', 'in-linesafterpattern',
        //    'Specify pattern to search the "lines-after" lines on (used with --linesafter)',
        //    function(x, settings) { settings.addInLinesAfterPattern(x); }),
        //new SearchOption('', 'out-linesbeforepattern',
        //    'Specify pattern to filter the "lines-before" lines on (used with' +
        //        ' --linesbefore)',
        //    function(x, settings) { settings.addOutLinesBeforePattern(x); }),
        //new SearchOption('', 'in-linesbeforepattern',
        //    'Specify pattern to search the "lines-before" lines on (used with' +
        //        ' --linesbefore)',
        //    function(x, settings) { settings.addInLinesBeforePattern(x); }),
        new SearchOption('s', 'search',
            'Specify search pattern',
            function(x, settings) { settings.addSearchPattern(x); }),
        new SearchOption('x', 'ext',
            'Specify extension for files to include in search',
            function(x, settings) { settings.addInExtension(x); }),
        new SearchOption('X', 'extfilter',
            'Specify extension for files to exclude from search',
            function(x, settings) { settings.addOutExtension(x); })
    ];

    var flagOptions = [
        new SearchOption('1', 'firstmatch',
            'Capture only the first match for a file+search combination',
            function(settings) { settings.firstMatch = true; }),
        new SearchOption('a', 'allmatches',
            'Capture all matches*',
            function(settings) { settings.firstMatch = false; }),
        new SearchOption('', 'debug',
            'Set output mode to debug',
            function(settings) { settings.debug = true; }),
        new SearchOption('h', 'help',
            'Print this usage and exit',
            function(settings) { settings.printUsage = true; }),
        new SearchOption('', 'listfiles',
            'Generate a list of the matching files after searching',
            function(settings) { settings.listFiles = true; }),
        new SearchOption('', 'listlines',
            'Generate a list of the matching lines after searching',
            function(settings) { settings.listLines = true; }),
        new SearchOption('m', 'multilinesearch',
            'Search files as single multi-line content block',
            function(settings) { settings.multilineSearch = true; }),
        new SearchOption('p', 'printmatches',
            'Print matches to stdout as found*',
            function(settings) { settings.printResults = true; }),
        new SearchOption('P', 'noprintmatches',
            'Suppress printing of matches to stdout',
            function(settings) { settings.printResults = false; }),
        new SearchOption('t', 'dotiming',
            'Time search execution',
            function(settings) { settings.doTiming = true; }),
        new SearchOption('v', 'verbose',
            'Set output mode to verbose',
            function(settings) { settings.verbose = true; }),
        new SearchOption('V', 'version',
            'Print version and exit',
            function(settings) { settings.printVersion = true; }),
        new SearchOption('z', 'searchcompressed',
            'Search compressed files (bz2, gz, tar, zip)*',
            function(settings) { settings.searchCompressed = true; }),
        new SearchOption('Z', 'nosearchcompressed',
            'Do not search compressed files (bz2, gz, tar, zip)',
            function(settings) { settings.searchCompressed = false; })
    ];

    var mapFromOptions = function (opts) {
        var optMap = {};
        for (var o in opts) {
            var opt = opts[o];
            if (opt.shortarg)
                optMap[opt.shortarg] = opt;
            optMap[opt.longarg] = opt;
        }
        return optMap;
    };

    var options = argOptions.concat(flagOptions);
    options.sort(function (a,b) {a.sortarg.localeCompare(b.sortarg);});
    var argMap = mapFromOptions(argOptions);
    var flagMap = mapFromOptions(flagOptions);

    this.searchSettingsFromArgs = function (args) {
        var settings = new SearchSettings();
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
            usage +=  optString + '  ' + optDescs[o] + '\n';
        }
        return usage;
    };


};

exports.SearchOptions = SearchOptions;
