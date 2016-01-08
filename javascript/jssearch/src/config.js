/*
 * config.js
 *
 * Configuration values
 */

"use strict";

var config = require('../../../shared/config.json');

var isWin = /^win/.test(process.platform);

var HOME_NAME = isWin ? 'USERPROFILE' : 'HOME';
var HOME = process.env[HOME_NAME];

exports.XSEARCHPATH = config.xsearchpath;
exports.SHAREDPATH = exports.XSEARCHPATH + '/shared';
exports.FILETYPESPATH = exports.SHAREDPATH + '/filetypes.xml';
exports.SEARCHOPTIONSPATH = exports.SHAREDPATH + '/searchoptions.xml';
