#import "SearchResult.h"

@implementation SearchResult

- (instancetype) initWithPattern:(NSString*)pattern
                            file:(SearchFile*)file
                         lineNum:(long)lineNum
                 matchStartIndex:(long)matchStartIndex
                   matchEndIndex:(long)matchEndIndex
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

- (NSString *) description {
    if (self.linesBefore.count > 0 || self.linesAfter.count > 0) {
        return [self multiLineToString];
    } else {
        return [self singleLineToString];
    }
}

- (NSString *) getFilePath {
    if (self.file != nil) {
        return [self.file description];
    } else {
        return @"<text>";
    }
}

- (NSString *) getRepeatingString:(NSString*)s count:(int)count {
    return [@"" stringByPaddingToLength:count withString:s startingAtIndex:0];
}

- (NSString *) singleLineToString {
    NSMutableString *s = [NSMutableString string];
    [s appendString:[self getFilePath]];
    if (self.lineNum > 0) {
        [s appendFormat:@": %lu: [%lu:%lu]: ", self.lineNum,
         self.matchStartIndex, self.matchEndIndex];
        [s appendString:[self.line stringByTrimmingCharactersInSet:[NSCharacterSet characterSetWithCharactersInString:@" \t\r\n"]]];
    } else {
        [s appendFormat:@" matches at [%lu:%lu]",
         self.matchStartIndex, self.matchEndIndex];
    }
    return [NSString stringWithString:s];
}

- (NSString *) multiLineToString {
    NSMutableString *s = [NSMutableString string];
    int sepLen = 80;
    [s appendFormat:@"%@\n", [self getRepeatingString:@"=" count:sepLen]];
    [s appendString:[self getFilePath]];
    [s appendFormat:@": %lu: [%lu:%lu]\n", self.lineNum,
     self.matchStartIndex, self.matchEndIndex];
    [s appendFormat:@"%@\n", [self getRepeatingString:@"-" count:sepLen]];
    
    long maxLineNum = self.lineNum + [self.linesAfter count];
    long maxNumLen = [[NSString stringWithFormat:@"%lu", maxLineNum] length];
    char cNumFormat[maxNumLen + 4];
    sprintf(cNumFormat, "%%%lulu", maxNumLen);
    NSString *numberFormat = [NSString stringWithUTF8String:cNumFormat];
    for (unsigned long i=0; i < [self.linesBefore count]; i++) {
        [s appendString:[self formatLineString:self.linesBefore[i]
                                       lineNum:self.lineNum - [self.linesBefore count] + i
                                  numberFormat:numberFormat prefix:@"  "]];
    }
    [s appendString:[self formatLineString:self.line
                                   lineNum:self.lineNum
                              numberFormat:numberFormat prefix:@"> "]];
    for (unsigned long i=0; i < [self.linesAfter count]; i++) {
        [s appendString:[self formatLineString:self.linesAfter[i]
                                       lineNum:self.lineNum + i + 1
                                  numberFormat:numberFormat prefix:@"  "]];
    }

    return [NSString stringWithString:s];
}

- (NSString *) formatLineString:(NSString*)line
                        lineNum:(long)lineNum
                   numberFormat:(NSString*)numberFormat
                         prefix:(NSString*)prefix {
    NSMutableString *s = [NSMutableString stringWithString:prefix];
    char cNumString[10];
    sprintf(cNumString, [numberFormat UTF8String], lineNum);
    NSString *numberString = [NSString stringWithUTF8String:cNumString];
    [s appendFormat:@"%@ | %@", numberString, line];
    return [NSString stringWithString:s];
}








@end
