/*
 * searchoptions.js
 *
 * defines the set of search options and provides functionality to define search settings from them
 */

const {ArgTokenType, ArgTokenizer, FileUtil, FindError, nameToSortBy} = require('jsfind');
const config = require('./config');
const {SearchError} = require('./searcherror');
const {SearchOption} = require('./searchoption');
const {SearchSettings} = require('./searchsettings');

class SearchOptions {
    'use strict'

    constructor() {
        // path included separately because it is not included as an option in findoptions.json
        this.argNameMap = {'path': 'path'};
        this.boolActionMap = {
            'allmatches':
                (b, settings) => { settings.firstMatch = !b; },
            'archivesonly':
                (b, settings) => { settings.archivesOnly = b; },
            'colorize':
                (b, settings) => { settings.colorize = b; },
            'debug':
                (b, settings) => { settings.debug = b; },
            'excludehidden':
                (b, settings) => { settings.includeHidden = !b; },
            'firstmatch':
                (b, settings) => { settings.firstMatch = b; },
            'followsymlinks':
                (b, settings) => { settings.followSymlinks = b; },
            'includehidden':
                (b, settings) => { settings.includeHidden = b; },
            'help':
                (b, settings) => { settings.printUsage = b; },
            'multilinesearch':
                (b, settings) => { settings.multilineSearch = b; },
            'nocolorize':
                (b, settings) => { settings.colorize = !b; },
            'nofollowsymlinks':
                (b, settings) => { settings.followSymlinks = !b; },
            'noprintdirs':
                (b, settings) => { settings.printDirs = !b; },
            'noprintfiles':
                (b, settings) => { settings.printFiles = !b; },
            'noprintlines':
                (b, settings) => { settings.printLines = !b; },
            'noprintmatches':
                (b, settings) => { settings.printResults = !b; },
            'norecursive':
                (b, settings) => { settings.recursive = !b; },
            'nosearcharchives':
                (b, settings) => { settings.searchArchives = !b; },
            'printdirs':
                (b, settings) => { settings.printDirs = b; },
            'printfiles':
                (b, settings) => { settings.printFiles = b; },
            'printlines':
                (b, settings) => { settings.printLines = b; },
            'printmatches':
                (b, settings) => { settings.printResults = b; },
            'recursive':
                (b, settings) => { settings.recursive = b; },
            'searcharchives':
                (b, settings) => { settings.searchArchives = b; },
            'sort-ascending':
                (b, settings) => { settings.sortDescending = !b; },
            'sort-caseinsensitive':
                (b, settings) => { settings.sortCaseInsensitive = b; },
            'sort-casesensitive':
                (b, settings) => { settings.sortCaseInsensitive = !b; },
            'sort-descending':
                (b, settings) => { settings.sortDescending = b; },
            'uniquelines':
                (b, settings) => { settings.uniqueLines = b; },
            'verbose':
                (b, settings) => { settings.verbose = b; },
            'version':
                (b, settings) => { settings.printVersion = b; }
        };
        this.stringActionMap = {
            'encoding':
                (s, settings) => { settings.textFileEncoding = s; },
            'in-archiveext':
                (s, settings) => { settings.addInArchiveExtensions(s); },
            'in-archivefilepattern':
                (s, settings) => { settings.addInArchiveFilePatterns(s); },
            'in-dirpattern':
                (s, settings) => { settings.addInDirPatterns(s); },
            'in-ext':
                (s, settings) => { settings.addInExtensions(s); },
            'in-filepattern':
                (s, settings) => { settings.addInFilePatterns(s); },
            'in-filetype':
                (s, settings) => { settings.addInFileTypes(s); },
            'in-linesafterpattern':
                (s, settings) => { settings.addInLinesAfterPatterns(s); },
            'in-linesbeforepattern':
                (s, settings) => { settings.addInLinesBeforePatterns(s); },
            'linesaftertopattern':
                (s, settings) => { settings.addLinesAfterToPatterns(s); },
            'linesafteruntilpattern':
                (s, settings) => { settings.addLinesAfterUntilPatterns(s); },
            'maxlastmod':
                (s, settings) => { settings.maxLastModFromString(s); },
            'minlastmod':
                (s, settings) => { settings.minLastModFromString(s); },
            'out-dirpattern':
                (s, settings) => { settings.addOutDirPatterns(s); },
            'out-archiveext':
                (s, settings) => { settings.addOutArchiveExtensions(s); },
            'out-archivefilepattern':
                (s, settings) => { settings.addOutArchiveFilePatterns(s); },
            'out-ext':
                (s, settings) => { settings.addOutExtensions(s); },
            'out-filepattern':
                (s, settings) => { settings.addOutFilePatterns(s); },
            'out-filetype':
                (s, settings) => { settings.addOutFileTypes(s); },
            'out-linesafterpattern':
                (s, settings) => { settings.addOutLinesAfterPatterns(s); },
            'out-linesbeforepattern':
                (s, settings) => { settings.addOutLinesBeforePatterns(s); },
            'path':
                (s, settings) => { settings.paths.push(s); },
            'searchpattern':
                (s, settings) => { settings.addSearchPatterns(s); },
            // 'settings-file':
            //     (s, settings) => { return this.updateSettingsFromFile(s, settings); },
            'sort-by':
                (s, settings) => { settings.sortBy = nameToSortBy(s); }
        };
        this.intActionMap = {
            'linesafter':
                (i, settings) => { settings.linesAfter = i; },
            'linesbefore':
                (i, settings) => { settings.linesBefore = i; },
            'maxdepth':
                (i, settings) => { settings.maxDepth = i; },
            'maxlinelength':
                (i, settings) => { settings.maxLineLength = i; },
            'maxsize':
                (i, settings) => { settings.maxSize = i; },
            'mindepth':
                (i, settings) => { settings.minDepth = i; },
            'minsize':
                (i, settings) => { settings.minSize = i; },
        };

        // the list of SearchOption objects (populated from JSON)
        this.options = [];
        (() => {
            let json = FileUtil.getFileContentsSync(config.SEARCH_OPTIONS_JSON_PATH, 'utf-8');
            let obj = JSON.parse(json);
            if (Object.prototype.hasOwnProperty.call(obj, 'searchoptions') && Array.isArray(obj.searchoptions)) {
                obj.searchoptions.forEach(so => {
                    let longArg = so.long;
                    this.argNameMap[longArg] = longArg;
                    let shortArg = '';
                    if (Object.prototype.hasOwnProperty.call(so, 'short')) {
                        shortArg = so.short;
                        this.argNameMap[shortArg] = longArg;
                    }
                    let desc = so.desc;
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
            } else throw new SearchError(`Invalid searchoptions file: ${config.SEARCH_OPTIONS_JSON_PATH}`);
            this.argTokenizer = new ArgTokenizer(this.options);
        })();
    }

    updateSettingsFromArgTokens(settings, argTokens) {
        let err = null;
        for (const argToken of argTokens) {
            if (err) break;
            if (argToken.type === ArgTokenType.Bool) {
                if (typeof argToken.value === 'boolean') {
                    this.boolActionMap[argToken.name](argToken.value, settings);
                } else {
                    err = new FindError(`Invalid value for option: ${argToken}`);
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
                            err = new FindError(`Invalid value for option: ${argToken}`);
                        }
                    });
                } else {
                    err = new FindError(`Invalid value for option: ${argToken}`);
                }
            } else if (argToken.type === ArgTokenType.Int) {
                if (typeof argToken.value === 'number') {
                    this.intActionMap[argToken.name](argToken.value, settings);
                } else {
                    err = new FindError(`Invalid value for option: ${argToken}`);
                }
            } else {
                err = new FindError(`Invalid option: ${argToken}`);
            }
        }
        return err;
    }

    updateSettingsFromJson(settings, json) {
        let { err, argTokens } = this.argTokenizer.tokenizeJson(json);
        if (!err) {
            err = this.updateSettingsFromArgTokens(settings, argTokens);
        }
        return err;
    }

    updateSettingsFromFile(settings, filePath) {
        let { err, argTokens } = this.argTokenizer.tokenizeFile(filePath);
        if (!err) {
            err = this.updateSettingsFromArgTokens(settings, argTokens);
        }
        return err;
    }

    updateSettingsFromArgs(settings, args) {
        let { err, argTokens } = this.argTokenizer.tokenizeArgs(args);
        if (!err) {
            err = this.updateSettingsFromArgTokens(settings, argTokens);
        }
        return err;
    }

    settingsFromArgs(args, cb) {
        let settings = new SearchSettings();
        // default printFiles to true since running as cli
        settings.printResults = true;
        let err = this.updateSettingsFromArgs(settings, args);
        cb(err, settings);
    }

    optCmp(o1, o2) {
        const a = o1.sortArg;
        const b = o2.sortArg;
        return a.localeCompare(b);
    }

    getUsageString() {
        let usage = 'Usage:\n jssearch [options] -s <searchpattern> <path> [<path> ...]\n\n';
        usage += 'Options:\n';
        let optStrings = [];
        let optDescs = [];
        let longest = 0;
        this.options.sort(this.optCmp);
        this.options.forEach(opt => {
            let optString = ' ';
            if (opt.shortArg)
                optString += '-' + opt.shortArg + ',';
            optString += '--' + opt.longArg;
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
    }

    usageWithCode(exitCode) {
        console.log(this.getUsageString());
        process.exit(exitCode);
    }

    usage() {
        this.usageWithCode(0);
    }
}

exports.SearchOptions = SearchOptions;
