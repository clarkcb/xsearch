/*
 * config.js
 *
 * Configuration values
 */

var isWin = /^win/.test(process.platform);

var HOME_NAME = isWin ? 'USERPROFILE' : 'HOME';
var HOME = process.env[HOME_NAME];
var XSEARCHPATH = HOME + '/src/git/xsearch';
exports.SHAREDPATH = XSEARCHPATH + '/shared';
exports.FILETYPESPATH = exports.SHAREDPATH + '/filetypes.xml';
exports.SEARCHOPTIONSPATH = exports.SHAREDPATH + '/searchoptions.xml';
