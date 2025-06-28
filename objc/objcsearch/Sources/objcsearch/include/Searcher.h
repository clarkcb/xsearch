#ifndef Searcher_h
#define Searcher_h

#import <Foundation/Foundation.h>
#import "FileResult.h"
#import "FileTypes.h"
#import "Finder.h"
#import "SearchResult.h"
#import "SearchResultFormatter.h"
#import "SearchSettings.h"

@interface Searcher : NSObject

@property FileTypes *fileTypes;
@property Finder *finder;
@property SearchSettings *settings;
@property NSStringEncoding textFileEncoding;

- (instancetype) initWithSettings:(SearchSettings*)settings error:(NSError**)error;
- (NSArray<SearchResult*>*) search:(NSError**)error;
- (NSArray<SearchResult*>*) searchFile:(FileResult*)fr error:(NSError**)error;
- (NSArray<SearchResult*>*) searchMultiLineString:(NSString*)s error:(NSError**)error;
- (void) printMatchingDirs:(NSArray<SearchResult*>*)searchResults formatter:(SearchResultFormatter*)formatter;
- (void) printMatchingFiles:(NSArray<SearchResult*>*)searchResults formatter:(SearchResultFormatter*)formatter;
- (void) printMatchingLines:(NSArray<SearchResult*>*)searchResults formatter:(SearchResultFormatter*)formatter;
- (void) printSearchResults:(NSArray<SearchResult*>*)searchResults formatter:(SearchResultFormatter*)formatter;

@end

#endif /* Searcher_h */
