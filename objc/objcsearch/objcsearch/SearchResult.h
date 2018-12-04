#import <Foundation/Foundation.h>
#import "SearchFile.h"

@interface SearchResult : NSObject

@property NSString *searchPattern;
@property SearchFile *file;
@property long lineNum;
@property long matchStartIndex;
@property long matchEndIndex;
@property NSString *line;
@property NSArray<NSString*> *linesBefore;
@property NSArray<NSString*> *linesAfter;

- (instancetype) initWithPattern:(NSString*)pattern
                            file:(SearchFile*)file
                         lineNum:(long)lineNum
                 matchStartIndex:(long)matchStartIndex
                   matchEndIndex:(long)matchEndIndex
                            line:(NSString*)line
                     linesBefore:(NSArray<NSString*>*)linesBefore
                      linesAfter:(NSArray<NSString*>*)linesAfter;

- (NSString *) description;

@end
