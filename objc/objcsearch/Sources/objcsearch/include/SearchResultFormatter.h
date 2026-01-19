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
@property (nonatomic, copy) NSString* ( ^ formatMatch ) ( NSString * );

- (instancetype) initWithSettings:(SearchSettings*)settings;

- (NSString *) formatLineWithColor:(NSString*)line;

- (NSString *) formatMatchWithColor:(NSString*)match;

- (NSString *) format:(SearchResult*)result;

// These might be temporary just to test the methods
- (NSString *) colorize:(NSString*)s matchStartIndex:(long)matchStartIndex matchEndIndex:(long)matchEndIndex color:(Color)color;
- (NSString *) formatMatchingLine:(SearchResult*)result;
@end

#endif /* SearchResultFormatter_h */
