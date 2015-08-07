/// <reference path="../typings/node/node.d.ts"/>
/*
 * searchoption.js
 *
 * encapsulates a search option
 */

interface SettingsFunc {
    (): void;
}

class SearchOption {
    shortarg: string;
    longarg: string;
    desc: string;
    func: SettingsFunc;
    public sortarg: string;

    constructor(shortarg: string, longarg: string, desc: string, func: SettingsFunc) {
        this.shortarg = shortarg;
        this.longarg = longarg;
        this.desc = desc;
        this.func = func;
        this.sortarg = this.getSortarg();
    }

    private getSortarg(): string {
        if (this.shortarg)
            return this.shortarg.toLowerCase() + 'a' + this.longarg.toLowerCase();
        return this.longarg.toLowerCase();
    }
}

exports.SearchOption = SearchOption;
