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
        if (that.shortarg) {
            return that.shortarg;
        } else {
            return that.longarg;
        }
    })();
}

exports.SearchOption = SearchOption;
