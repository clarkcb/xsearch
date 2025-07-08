#import "SearchResultSorter.h"

@implementation SearchResultSorter

- (instancetype) initWithSettings:(SearchSettings*)settings {
    self = [super init];
    if (self) {
        self.settings = settings;
    }
    return self;
}

- (NSComparisonResult (^)(SearchResult*, SearchResult*)) getSearchResultComparator {
    if (self.settings.sortDescending) {
        if (self.settings.sortBy == SortByFileName) {
            return ^NSComparisonResult(SearchResult *sr1, SearchResult *sr2) {
                return [sr2 compareByName:sr1 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else if (self.settings.sortBy == SortByFileSize) {
            return ^NSComparisonResult(SearchResult *sr1, SearchResult *sr2) {
                return [sr2 compareBySize:sr1 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else if (self.settings.sortBy == SortByFileType) {
            return ^NSComparisonResult(SearchResult *sr1, SearchResult *sr2) {
                return [sr2 compareByType:sr1 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else if (self.settings.sortBy == SortByLastMod) {
            return ^NSComparisonResult(SearchResult *sr1, SearchResult *sr2) {
                return [sr2 compareByLastMod:sr1 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else {
            return ^NSComparisonResult(SearchResult *sr1, SearchResult *sr2) {
                return [sr2 compareByPath:sr1 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        }
    } else {
        if (self.settings.sortBy == SortByFileName) {
            return ^NSComparisonResult(SearchResult *sr1, SearchResult *sr2) {
                return [sr1 compareByName:sr2 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else if (self.settings.sortBy == SortByFileSize) {
            return ^NSComparisonResult(SearchResult *sr1, SearchResult *sr2) {
                return [sr1 compareBySize:sr2 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else if (self.settings.sortBy == SortByFileType) {
            return ^NSComparisonResult(SearchResult *sr1, SearchResult *sr2) {
                return [sr1 compareByType:sr2 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else if (self.settings.sortBy == SortByLastMod) {
            return ^NSComparisonResult(SearchResult *sr1, SearchResult *sr2) {
                return [sr1 compareByLastMod:sr2 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        } else {
            return ^NSComparisonResult(SearchResult *sr1, SearchResult *sr2) {
                return [sr1 compareByPath:sr2 caseInsensitive:self.settings.sortCaseInsensitive];
            };
        }
    }
}

- (NSArray<SearchResult*>*) sort:(NSArray<SearchResult*>*)searchResults {
    return [searchResults sortedArrayUsingComparator:[self getSearchResultComparator]];
}

@end
