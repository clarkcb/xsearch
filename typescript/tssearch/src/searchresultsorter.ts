/*
 * searchresultsorter.ts
 *
 * SearchResultSorter class provides sorting of search results
 */

import {FileResultSorter, SortBy} from 'tsfind';
import {SearchResult} from './searchresult';
import {SearchSettings} from './searchsettings';

"use strict";

export class SearchResultSorter {
    settings: SearchSettings;
    fileSorter: FileResultSorter;

    constructor(settings: SearchSettings) {
        this.settings = settings;
        this.fileSorter = new FileResultSorter(settings.getFindSettings());
    }

    private cmpBySearchFields(sr1: SearchResult, sr2: SearchResult) {
        if (sr1.lineNum === sr2.lineNum) {
            if (sr1.matchStartIndex === sr2.matchStartIndex) {
                return sr1.matchEndIndex - sr2.matchEndIndex;
            }
            return sr1.matchStartIndex - sr2.matchStartIndex;
        }
        return sr1.lineNum - sr2.lineNum;
    }

    public cmpSearchResultsByPath(sr1: SearchResult, sr2: SearchResult): number {
        if (sr1.file && sr2.file) {
            const fileCmp = this.fileSorter.cmpFileResultsByPath(sr1.file!, sr2.file!);
            if (fileCmp !== 0) {
                return fileCmp;
            }
        }
        return this.cmpBySearchFields(sr1, sr2);
    }

    public cmpSearchResultsByName(sr1: SearchResult, sr2: SearchResult): number {
        if (sr1.file && sr2.file) {
            const fileCmp = this.fileSorter.cmpFileResultsByName(sr1.file!, sr2.file!);
            if (fileCmp !== 0) {
                return fileCmp;
            }
        }
        return this.cmpBySearchFields(sr1, sr2);
    }

    public cmpSearchResultsBySize(sr1: SearchResult, sr2: SearchResult): number {
        if (sr1.file && sr2.file) {
            const fileCmp = this.fileSorter.cmpFileResultsBySize(sr1.file!, sr2.file!);
            if (fileCmp !== 0) {
                return fileCmp;
            }
        }
        return this.cmpBySearchFields(sr1, sr2);
    }

    public cmpSearchResultsByType(sr1: SearchResult, sr2: SearchResult): number {
        if (sr1.file && sr2.file) {
            const fileCmp = this.fileSorter.cmpFileResultsByType(sr1.file!, sr2.file!);
            if (fileCmp !== 0) {
                return fileCmp;
            }
        }
        return this.cmpBySearchFields(sr1, sr2);
    }

    public cmpSearchResultsByLastMod(sr1: SearchResult, sr2: SearchResult): number {
        if (sr1.file && sr2.file) {
            const fileCmp = this.fileSorter.cmpFileResultsByLastMod(sr1.file!, sr2.file!);
            if (fileCmp !== 0) {
                return fileCmp;
            }
        }
        return this.cmpBySearchFields(sr1, sr2);
    }

    public getSearchResultComparator(): (a: SearchResult, b: SearchResult) => number {
        if (this.settings.sortDescending) {
            switch (this.settings.sortBy) {
                case SortBy.FileName:
                    return (a, b) => this.cmpSearchResultsByName(b, a);
                case SortBy.FileSize:
                    return (a, b) => this.cmpSearchResultsBySize(b, a);
                case SortBy.FileType:
                    return (a, b) => this.cmpSearchResultsByType(b, a);
                case SortBy.LastMod:
                    return (a, b) => this.cmpSearchResultsByLastMod(b, a);
                default:
                    return (a, b) => this.cmpSearchResultsByPath(b, a);
            }
        }
        switch (this.settings.sortBy) {
            case SortBy.FileName:
                return (a, b) => this.cmpSearchResultsByName(a, b);
            case SortBy.FileSize:
                return (a, b) => this.cmpSearchResultsBySize(a, b);
            case SortBy.FileType:
                return (a, b) => this.cmpSearchResultsByType(a, b);
            case SortBy.LastMod:
                return (a, b) => this.cmpSearchResultsByLastMod(a, b);
            default:
                return (a, b) => this.cmpSearchResultsByPath(a, b);
        }
    }

    public sort(searchResults: SearchResult[]): void {
        const sortComparator = this.getSearchResultComparator();
        searchResults.sort(sortComparator);
    }
}
