#import <Foundation/Foundation.h>
#import "SearchFile.h"

@interface SearchResult : NSObject

@property NSString *searchPattern;
@property SearchFile *file;
@property unsigned long lineNum;
@property unsigned long matchStartIndex;
@property unsigned long matchEndIndex;
@property NSString *line;
@property NSArray<NSString*> *linesBefore;
@property NSArray<NSString*> *linesAfter;

- (instancetype) initWithPattern:(NSString*)pattern
                            file:(SearchFile*)file
                         lineNum:(unsigned long)lineNum
                 matchStartIndex:(unsigned long)matchStartIndex
                   matchEndIndex:(unsigned long)matchEndIndex
                            line:(NSString*)line
                     linesBefore:(NSArray<NSString*>*)linesBefore
                      linesAfter:(NSArray<NSString*>*)linesAfter;

- (NSString *) getFilePath;
@end
