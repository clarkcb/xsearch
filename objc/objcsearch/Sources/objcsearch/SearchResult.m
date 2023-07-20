#import "SearchResult.h"
#import "color.h"

@implementation SearchResult

- (instancetype) initWithPattern:(NSString*)pattern
                            file:(FileResult*)file
                         lineNum:(unsigned long)lineNum
                 matchStartIndex:(unsigned long)matchStartIndex
                   matchEndIndex:(unsigned long)matchEndIndex
                            line:(NSString*)line
                     linesBefore:(NSArray<NSString*>*)linesBefore
                      linesAfter:(NSArray<NSString*>*)linesAfter
{
    self = [super init];
    if (self) {
        self.searchPattern = pattern;
        self.file = file;
        self.lineNum = lineNum;
        self.matchStartIndex = matchStartIndex;
        self.matchEndIndex = matchEndIndex;
        self.line = line;
        self.linesBefore = linesBefore;
        self.linesAfter = linesAfter;
    }
    return self;
}

- (NSString *) getFilePath {
    if (self.file != nil) {
        return [self.file description];
    } else {
        return @"<text>";
    }
}

@end
