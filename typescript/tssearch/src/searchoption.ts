/*
 * searchoption.js
 *
 * encapsulates a search option
 */

"use strict";

export class SearchOption {
    shortarg: string;
    longarg: string;
    desc: string;
    public sortarg: string;

    constructor(shortarg: string, longarg: string, desc: string) {
        this.shortarg = shortarg;
        this.longarg = longarg;
        this.desc = desc;
        this.sortarg = this.getSortarg();
    }

    private getSortarg(): string {
        if (this.shortarg)
            return this.shortarg.toLowerCase() + 'a' + this.longarg.toLowerCase();
        return this.longarg.toLowerCase();
    }
}
