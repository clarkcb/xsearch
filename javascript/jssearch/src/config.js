/*
 * config.js
 *
 * Configuration values
 */

"use strict";

const config = require('../data/config.json');

// const isWin = /^win/.test(process.platform);

// const HOME_NAME = isWin ? 'USERPROFILE' : 'HOME';
// const HOME = process.env[HOME_NAME];

// use XSEARCH_PATH env var if defined
if (process.env.XSEARCH_PATH) {
    config.xsearchpath = process.env.XSEARCH_PATH;
}

exports.XSEARCHPATH = config.xsearchpath;
exports.SHAREDPATH = exports.XSEARCHPATH + '/shared';
// exports.FILETYPESJSONPATH = __dirname + '/../data/filetypes.json';
exports.SEARCHOPTIONSJSONPATH = __dirname + '/../data/searchoptions.json';
