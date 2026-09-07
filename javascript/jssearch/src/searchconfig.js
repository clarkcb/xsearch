/*
 * searchconfig.js
 *
 * Configuration values
 */

'use strict';

const { FindConfig } = require('jsfind');
const path = require('path');

const isWin = /^win/.test(process.platform);

const HOME_NAME = isWin ? 'USERPROFILE' : 'HOME';
const HOME = process.env[HOME_NAME];

class SearchConfig extends FindConfig {
  constructor() {
    super();
    const defaultXSearchConfigDir = path.join(HOME, '.config', 'xsearch');
    const xsearchConfigDir = process.env.XSEARCH_CONFIG_DIR
      ? process.env.XSEARCH_CONFIG_DIR
      : defaultXSearchConfigDir;
    const defaultXSearchPath = path.join(HOME, 'src', 'xsearch');
    const xsearchPath = process.env.XSEARCH_PATH ? process.env.XSEARCH_PATH : defaultXSearchPath;
    const jssearchPath = path.join(xsearchPath, 'javascript', 'jssearch');
    const dataPath = path.join(jssearchPath, 'data');
    const searchOptionsPath = path.join(dataPath, 'searchoptions.json');
    const defaultSearchSettingsPath = path.join(xsearchConfigDir, 'settings.json');

    this.xsearchPath = xsearchPath;
    this.searchOptionsPath = searchOptionsPath;
    this.defaultSearchSettingsPath = defaultSearchSettingsPath;
  }
}

exports.SearchConfig = SearchConfig;
