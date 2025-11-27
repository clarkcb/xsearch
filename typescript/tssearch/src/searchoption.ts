/*
 * searchoption.js
 *
 * encapsulates a search option
 */

"use strict";

import {ArgTokenType, Option} from 'tsfind';

export class SearchOption implements Option {
    shortArg: string;
    longArg: string;
    desc: string;
    argType: ArgTokenType;
    public sortArg: string;

    constructor(shortArg: string, longArg: string, desc: string, argType: ArgTokenType) {
        this.shortArg = shortArg;
        this.longArg = longArg;
        this.desc = desc;
        this.argType = argType;
        this.sortArg = this.getSortArg();
    }

    private getSortArg(): string {
        if (this.shortArg)
            return this.shortArg.toLowerCase() + 'a' + this.longArg.toLowerCase();
        return this.longArg.toLowerCase();
    }
}
