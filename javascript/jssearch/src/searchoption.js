/*
 * searchoption.js
 *
 * encapsulates a search option
 */

class SearchOption {
    'use strict'

    constructor(shortarg, longarg, desc, func) {
        this.shortarg = shortarg;
        this.longarg = longarg;
        this.desc = desc;
        this.func = func;

        this.sortarg = (() => {
            if (this.shortarg)
                return this.shortarg.toLowerCase() + 'a' + this.longarg.toLowerCase();
            return this.longarg.toLowerCase();
        })();
    }
}

exports.SearchOption = SearchOption;
