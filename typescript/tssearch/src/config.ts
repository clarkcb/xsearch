/*
 * config.ts
 *
 * Configuration values
 */

"use strict";

const config = require('../../../shared/config.json');

const isWin: boolean = /^win/.test(process.platform);

const HOME_NAME: string = isWin ? 'USERPROFILE' : 'HOME';
export const HOME: string = process.env[HOME_NAME] || '';

export const XSEARCHPATH: string = config.xsearchpath;
export const SHAREDPATH: string = XSEARCHPATH + '/shared';
export const FILETYPESJSONPATH: string = __dirname + '/../data/filetypes.json';
export const SEARCHOPTIONSJSONPATH: string = __dirname + '/../data/searchoptions.json';
