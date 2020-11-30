/*
 * config.js
 *
 * Configuration values
 */

"use strict";

const config = require('../../../shared/config.json');

const isWin = /^win/.test(process.platform);

const HOME_NAME = isWin ? 'USERPROFILE' : 'HOME';
const HOME = process.env[HOME_NAME];

exports.XSEARCHPATH = config.xsearchpath;
exports.SHAREDPATH = exports.XSEARCHPATH + '/shared';
exports.FILETYPESJSONPATH = __dirname + '/../data/filetypes.json';
exports.SEARCHOPTIONSJSONPATH = __dirname + '/../data/searchoptions.json';
