/*
 * searchresultsorter.js
 *
 * SearchResultSorter class provides sorting of search results
 */
const {FileResultSorter, SortBy} = require('jsfind');


class SearchResultSorter {
  'use strict'

  constructor (settings) {
    this.settings = settings;
    this.fileSorter = new FileResultSorter(settings);
  }

  cmpBySearchFields(sr1, sr2) {
    if (sr1.lineNum === sr2.lineNum) {
      if (sr1.matchStartIndex === sr2.matchStartIndex) {
        return sr1.matchEndIndex - sr2.matchEndIndex;
      }
      return sr1.matchStartIndex - sr2.matchStartIndex;
    }
    return sr1.lineNum - sr2.lineNum;
  }

  cmpSearchResultsByPath(sr1, sr2) {
    const fileCmp = this.fileSorter.cmpFileResultsByPath(sr1.file, sr2.file);
    if (fileCmp === 0) {
      return this.cmpBySearchFields(sr1, sr2);
    }
    return fileCmp;
  }

  cmpSearchResultsByName(sr1, sr2) {
    const fileCmp = this.fileSorter.cmpFileResultsByName(sr1.file, sr2.file);
    if (fileCmp === 0) {
      return this.cmpBySearchFields(sr1, sr2);
    }
    return fileCmp;
  }

  cmpSearchResultsBySize(sr1, sr2) {
    const fileCmp = this.fileSorter.cmpFileResultsBySize(sr1.file, sr2.file);
    if (fileCmp === 0) {
      return this.cmpBySearchFields(sr1, sr2);
    }
    return fileCmp;
  }

  cmpSearchResultsByType(sr1, sr2) {
    const fileCmp = this.fileSorter.cmpFileResultsByType(sr1.file, sr2.file);
    if (fileCmp === 0) {
      return this.cmpBySearchFields(sr1, sr2);
    }
    return fileCmp;
  }

  cmpSearchResultsByLastMod(sr1, sr2) {
    const fileCmp = this.fileSorter.cmpFileResultsByLastMod(sr1.file, sr2.file);
    if (fileCmp === 0) {
      return this.cmpBySearchFields(sr1, sr2);
    }
    return fileCmp;
  }

  getSearchResultComparator() {
    if (this.settings.sortDescending) {
      switch (this.settings.sortBy) {
        case SortBy.FILENAME:
          return (a, b) => this.cmpSearchResultsByName(b, a);
        case SortBy.FILESIZE:
          return (a, b) => this.cmpSearchResultsBySize(b, a);
        case SortBy.FILETYPE:
          return (a, b) => this.cmpSearchResultsByType(b, a);
        case SortBy.LASTMOD:
          return (a, b) => this.cmpSearchResultsByLastMod(b, a);
        default:
          return (a, b) => this.cmpSearchResultsByPath(b, a);
      }
    }
    switch (this.settings.sortBy) {
      case SortBy.FILENAME:
        return (a, b) => this.cmpSearchResultsByName(a, b);
      case SortBy.FILESIZE:
        return (a, b) => this.cmpSearchResultsBySize(a, b);
      case SortBy.FILETYPE:
        return (a, b) => this.cmpSearchResultsByType(a, b);
      case SortBy.LASTMOD:
        return (a, b) => this.cmpSearchResultsByLastMod(a, b);
      default:
        return (a, b) => this.cmpSearchResultsByPath(a, b);
    }
  }

  sort(searchResults) {
    let sortComparator = this.getSearchResultComparator();
    searchResults.sort(sortComparator);
  }

}

exports.SearchResultSorter = SearchResultSorter;

