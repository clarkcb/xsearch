/// <reference path="../typings/node/node.d.ts"/>
/*
 * config.ts
 *
 * Configuration values
 */

var isWin: boolean = /^win/.test(process.platform);

var HOME_NAME: string = isWin ? 'USERPROFILE' : 'HOME';
var HOME: string = process.env[HOME_NAME];
exports.XSEARCHPATH = HOME + '/src/xsearch';
exports.SHAREDPATH = exports.XSEARCHPATH + '/shared';
exports.FILETYPESPATH = exports.SHAREDPATH + '/filetypes.xml';
exports.SEARCHOPTIONSPATH = exports.SHAREDPATH + '/searchoptions.xml';
