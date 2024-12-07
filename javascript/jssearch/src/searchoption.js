/*
 * searchoption.js
 *
 * encapsulates a search option
 */

class SearchOption {
    'use strict'

    constructor(shortArg, longArg, desc) {
        this.shortArg = shortArg;
        this.longArg = longArg;
        this.desc = desc;

        this.sortArg = (() => {
            if (this.shortArg)
                return this.shortArg.toLowerCase() + 'a' + this.longArg.toLowerCase();
            return this.longArg.toLowerCase();
        })();
    }
}

exports.SearchOption = SearchOption;
