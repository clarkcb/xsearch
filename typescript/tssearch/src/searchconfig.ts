/*
 * config.ts
 *
 * Configuration values
 */

'use strict';

import { FindConfig } from 'tsfind';

import path from 'path';

const isWin: boolean = /^win/.test(process.platform);

const HOME_NAME: string = isWin ? 'USERPROFILE' : 'HOME';
export const HOME: string = process.env[HOME_NAME] || '';

export const XSEARCH_PATH: string = process.env.XSEARCH_PATH
  ? process.env.XSEARCH_PATH
  : `${HOME}/src/xsearch`;
export const SHARED_PATH: string = `${XSEARCH_PATH}/shared`;
const TSSEARCH_PATH = `${XSEARCH_PATH}/typescript/tssearch`;
const DATA_PATH = `${TSSEARCH_PATH}/data`;
export const SEARCH_OPTIONS_JSON_PATH: string = `${DATA_PATH}/searchoptions.json`;
export const DEFAULT_SEARCH_SETTINGS_PATH: string = path.join(
  HOME,
  '.config',
  'xfind',
  'settings.json',
);

export class SearchConfig extends FindConfig {
  searchOptionsPath: string;
  defaultSearchSettingsPath: string;

  constructor() {
    super();
    const defaultXSearchConfigDir = path.join(HOME, '.config', 'xsearch');
    const xsearchConfigDir = process.env.XSEARCH_CONFIG_DIR
      ? process.env.XSEARCH_CONFIG_DIR
      : defaultXSearchConfigDir;
    const defaultXSearchPath = path.join(HOME, 'src', 'xsearch');
    const xsearchPath = process.env.XSEARCH_PATH ? process.env.XSEARCH_PATH : defaultXSearchPath;
    const tssearchPath = path.join(xsearchPath, 'typescript', 'tssearch');
    const dataPath = path.join(tssearchPath, 'data');
    const searchOptionsPath = path.join(dataPath, 'searchoptions.json');
    const defaultSearchSettingsPath = path.join(xsearchConfigDir, 'settings.json');

    this.searchOptionsPath = searchOptionsPath;
    this.defaultSearchSettingsPath = defaultSearchSettingsPath;
  }
}
