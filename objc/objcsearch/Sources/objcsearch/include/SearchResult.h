#ifndef SearchResult_h
#define SearchResult_h

#import <Foundation/Foundation.h>
#import "FileResult.h"

@interface SearchResult : NSObject

@property NSString *searchPattern;
@property FileResult *file;
@property unsigned long lineNum;
@property unsigned long matchStartIndex;
@property unsigned long matchEndIndex;
@property NSString *line;
@property NSArray<NSString*> *linesBefore;
@property NSArray<NSString*> *linesAfter;

- (instancetype) initWithPattern:(NSString*)pattern
                            file:(FileResult*)file
                         lineNum:(unsigned long)lineNum
                 matchStartIndex:(unsigned long)matchStartIndex
                   matchEndIndex:(unsigned long)matchEndIndex
                            line:(NSString*)line
                     linesBefore:(NSArray<NSString*>*)linesBefore
                      linesAfter:(NSArray<NSString*>*)linesAfter;

- (NSString *) getFilePath;
- (NSComparisonResult)compareByPath:(SearchResult *)otherSearchResult caseInsensitive:(BOOL)caseInsensitive;
- (NSComparisonResult)compareByName:(SearchResult *)otherSearchResult caseInsensitive:(BOOL)caseInsensitive;
- (NSComparisonResult)compareBySize:(SearchResult *)otherSearchResult caseInsensitive:(BOOL)caseInsensitive;
- (NSComparisonResult)compareByType:(SearchResult *)otherSearchResult caseInsensitive:(BOOL)caseInsensitive;
- (NSComparisonResult)compareByLastMod:(SearchResult *)otherSearchResult caseInsensitive:(BOOL)caseInsensitive;

@end

#endif /* SearchResult_h */
