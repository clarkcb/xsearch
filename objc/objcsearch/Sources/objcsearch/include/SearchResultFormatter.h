#ifndef SearchResultFormatter_h
#define SearchResultFormatter_h

#import <Foundation/Foundation.h>
#import "FileResultFormatter.h"
#import "SearchResult.h"
#import "SearchSettings.h"

@interface SearchResultFormatter : NSObject

@property SearchSettings *settings;
@property FileResultFormatter *fileFormatter;
@property (nonatomic, copy) NSString* ( ^ formatLine ) ( NSString * );

- (instancetype) initWithSettings:(SearchSettings*)settings;

- (NSString *) formatLineWithColor:(NSString*)line;

- (NSString *) format:(SearchResult*)result;

// These might be temporary just to test the methods
- (NSString *) colorize:(NSString*)s matchStartIndex:(long)matchStartIndex matchEndIndex:(long)matchEndIndex;
- (NSString *) formatMatchingLine:(SearchResult*)result;
@end

#endif /* SearchResultFormatter_h */
