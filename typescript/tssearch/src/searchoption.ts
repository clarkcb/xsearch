/*
 * searchoption.js
 *
 * encapsulates a search option
 */

"use strict";

export class SearchOption {
    shortArg: string;
    longArg: string;
    desc: string;
    public sortArg: string;

    constructor(shortarg: string, longarg: string, desc: string) {
        this.shortArg = shortarg;
        this.longArg = longarg;
        this.desc = desc;
        this.sortArg = this.getSortArg();
    }

    private getSortArg(): string {
        if (this.shortArg)
            return this.shortArg.toLowerCase() + 'a' + this.longArg.toLowerCase();
        return this.longArg.toLowerCase();
    }
}
