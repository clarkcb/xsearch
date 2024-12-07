/*
 * config.ts
 *
 * Configuration values
 */

'use strict';

const isWin: boolean = /^win/.test(process.platform);

const HOME_NAME: string = isWin ? 'USERPROFILE' : 'HOME';
export const HOME: string = process.env[HOME_NAME] || '';

export const XSEARCH_PATH: string = process.env.XSEARCH_PATH ? process.env.XSEARCH_PATH : `${HOME}/src/xsearch`;
export const SHARED_PATH: string = `${XSEARCH_PATH}/shared`;
const TSSEARCH_PATH = `${XSEARCH_PATH}/typescript/tssearch`;
const DATA_PATH = `${TSSEARCH_PATH}/data`;
export const SEARCH_OPTIONS_JSON_PATH: string = `${DATA_PATH}/searchoptions.json`;
