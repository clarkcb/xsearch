/*
 * config.js
 *
 * Configuration values
 */

var isWin = /^win/.test(process.platform);

var HOME_NAME = isWin ? 'USERPROFILE' : 'HOME';
var HOME = process.env[HOME_NAME];
var XSEARCHPATH = HOME + '/src/git/xsearch';
var SHAREDPATH = XSEARCHPATH + '/shared';
exports.FILETYPESPATH = SHAREDPATH + '/filetypes.xml';
exports.SEARCHOPTIONSPATH = SHAREDPATH + '/searchoptions.xml';
