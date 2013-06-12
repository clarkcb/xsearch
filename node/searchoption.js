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
        var sortarg = '';
        if (that.shortarg) {
            sortarg += that.shortarg.substring(0,1).toLowerCase() + 'a';
        }
        sortarg += that.longarg.toLowerCase();
        return sortarg;
    })();
}

exports.SearchOption = SearchOption;
