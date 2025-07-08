#ifndef SearchResultSorter_h
#define SearchResultSorter_h

#import <Foundation/Foundation.h>
#import "FileResultSorter.h"
#import "SearchResult.h"
#import "SearchSettings.h"

@interface SearchResultSorter : NSObject

@property SearchSettings *settings;

- (instancetype) initWithSettings:(SearchSettings*)settings;

- (NSComparisonResult (^)(SearchResult*, SearchResult*)) getSearchResultComparator;

- (NSArray<SearchResult*>*) sort:(NSArray<SearchResult*>*)searchResults;

@end

#endif /* SearchResultSorter_h */
