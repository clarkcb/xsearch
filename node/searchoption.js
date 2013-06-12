/*
 * searchoption.js
 *
 * encapsulates a search option
 */

function SearchOption(shortarg, longarg, desc, func) {
    var that = this;
    this.shortarg = shortarg;
    this.longarg = longarg;
    this.desc = desc;
    this.func = func;

    this.sortarg = (function () {
        if (that.shortarg)
            return that.shortarg.toLowerCase() + 'a' + that.longarg.toLowerCase();
        return that.longarg.toLowerCase();
    })();
}

exports.SearchOption = SearchOption;
