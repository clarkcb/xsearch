/*
 * searcherror.js
 *
 * custom error class for jssearch
 */

class SearchError extends Error {
    constructor(message) {
        super(message);
        this.name = "SearchError";
    }
}

exports.SearchError = SearchError;
