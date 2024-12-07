/*
 * config.js
 *
 * Configuration values
 */

"use strict";

const isWin = /^win/.test(process.platform);

const HOME_NAME = isWin ? 'USERPROFILE' : 'HOME';
const HOME = process.env[HOME_NAME];

exports.XSEARCH_PATH = process.env.XSEARCH_PATH ? process.env.XSEARCH_PATH : `${HOME}/src/xsearch`;
exports.SHARED_PATH = `${exports.XSEARCH_PATH}/shared`;
const JSSEARCH_PATH = `${exports.XSEARCH_PATH}/javascript/jssearch`;
const DATA_PATH = `${JSSEARCH_PATH}/data`;
exports.SEARCH_OPTIONS_JSON_PATH = `${DATA_PATH}/searchoptions.json`;
