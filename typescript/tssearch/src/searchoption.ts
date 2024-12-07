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

    constructor(shortArg: string, longArg: string, desc: string) {
        this.shortArg = shortArg;
        this.longArg = longArg;
        this.desc = desc;
        this.sortArg = this.getSortArg();
    }

    private getSortArg(): string {
        if (this.shortArg)
            return this.shortArg.toLowerCase() + 'a' + this.longArg.toLowerCase();
        return this.longArg.toLowerCase();
    }
}
