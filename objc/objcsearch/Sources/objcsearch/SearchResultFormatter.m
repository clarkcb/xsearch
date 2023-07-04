#import "FileUtil.h"
#import "SearchResultFormatter.h"

@implementation SearchResultFormatter

- (instancetype) initWithSettings:(SearchSettings*)settings
{
    self = [super init];
    if (self) {
        self.settings = settings;
    }
    return self;
}

- (NSString *) format:(SearchResult*)result {
    if (result.linesBefore.count > 0 || result.linesAfter.count > 0) {
        return [self multiLineFormat:result];
    } else {
        return [self singleLineFormat:result];
    }
}

- (NSString *) getRepeatingString:(NSString*)s count:(int)count {
    return [@"" stringByPaddingToLength:count withString:s startingAtIndex:0];
}

- (NSString *) singleLineFormat:(SearchResult*)result {
    NSMutableString *s = [NSMutableString string];
    [s appendString:[result getFilePath]];
    if (result.lineNum > 0) {
        [s appendFormat:@": %lu: [%lu:%lu]: ", result.lineNum,
         result.matchStartIndex, result.matchEndIndex];
//        [s appendString:[self.line stringByTrimmingCharactersInSet:[NSCharacterSet characterSetWithCharactersInString:@" \t\r\n"]]];
        [s appendString:[self formatMatchingLine:result]];
    } else {
        [s appendFormat:@" matches at [%lu:%lu]",
         result.matchStartIndex, result.matchEndIndex];
    }
    return [NSString stringWithString:s];
}

- (NSString *) formatMatchingLine:(SearchResult*)result {
    NSCharacterSet *whitespaceCharacterSet = [NSCharacterSet characterSetWithCharactersInString:@" \t\r\n"];
    NSMutableString *formatted = [NSMutableString string];
    int leadingWhitespaceCount = 0;
    while ([whitespaceCharacterSet characterIsMember:[result.line characterAtIndex:leadingWhitespaceCount]]) {
        leadingWhitespaceCount++;
    }
    [formatted appendString:[result.line stringByTrimmingCharactersInSet:whitespaceCharacterSet]];
    long formattedLength = [formatted length];
    long maxLineEndIndex = formattedLength - 1;
    long matchLength = result.matchEndIndex - result.matchStartIndex;
    long matchStartIndex = result.matchStartIndex - 1 - leadingWhitespaceCount;
    long matchEndIndex = matchStartIndex + matchLength;

    if (formattedLength > self.settings.maxLineLength) {
        long lineStartIndex = matchStartIndex;
        long lineEndIndex = lineStartIndex + matchLength;
        matchStartIndex = 0;
        matchEndIndex = matchLength;

        while (lineEndIndex > formattedLength - 1) {
            lineStartIndex--;
            lineEndIndex--;
            matchStartIndex++;
            matchEndIndex++;
        }

        formattedLength = lineEndIndex - lineStartIndex;
        while (formattedLength < self.settings.maxLineLength) {
            if (lineStartIndex > 0) {
                lineStartIndex--;
                matchStartIndex++;
                matchEndIndex++;
                formattedLength = lineEndIndex - lineStartIndex;
            }
            if (formattedLength < self.settings.maxLineLength && lineEndIndex < maxLineEndIndex) {
                lineEndIndex++;
            }
            formattedLength = lineEndIndex - lineStartIndex;
        }

        formatted = [NSMutableString stringWithFormat:@"%@", [formatted substringWithRange:NSMakeRange(lineStartIndex, formattedLength)]];

        if (lineStartIndex > 2) {
            formatted = [NSMutableString stringWithFormat:@"...%@", [formatted substringFromIndex:3]];
        }
        if (lineEndIndex < maxLineEndIndex - 3) {
            formatted = [NSMutableString stringWithFormat:@"%@...", [formatted substringToIndex:(formattedLength - 3)]];
        }
    }

    if (self.settings.colorize) {
        NSMutableString *c = [NSMutableString stringWithFormat:@"%@", [formatted substringToIndex:matchStartIndex]];
//        [c appendString:ANSI_GREEN];
        [c appendFormat:@"%s", ANSI_GREEN];
        [c appendString:[formatted substringWithRange:NSMakeRange(matchStartIndex, matchLength)]];
        [c appendString:ANSI_RESET];
        [c appendString:[formatted substringFromIndex:matchEndIndex]];
        formatted = [NSMutableString stringWithFormat:@"%@", c];
    }

    return [NSString stringWithString:formatted];
}

- (NSString *) multiLineFormat:(SearchResult*)result {
    NSMutableString *s = [NSMutableString string];
    int sepLen = 80;
    [s appendFormat:@"%@\n", [self getRepeatingString:@"=" count:sepLen]];
    [s appendString:[result getFilePath]];
    [s appendFormat:@": %lu: [%lu:%lu]\n", result.lineNum,
     result.matchStartIndex, result.matchEndIndex];
    [s appendFormat:@"%@\n", [self getRepeatingString:@"-" count:sepLen]];
    
    long maxLineNum = result.lineNum + [result.linesAfter count];
    long maxNumLen = [[NSString stringWithFormat:@"%lu", maxLineNum] length];
    char cNumFormat[maxNumLen + 4];
    sprintf(cNumFormat, "%%%lulu", maxNumLen);
    NSString *numberFormat = [NSString stringWithUTF8String:cNumFormat];
    for (unsigned long i=0; i < [result.linesBefore count]; i++) {
        [s appendString:[self formatLineString:result.linesBefore[i]
                                       lineNum:result.lineNum - [result.linesBefore count] + i
                                  numberFormat:numberFormat prefix:@"  "]];
    }
    [s appendString:[self formatLineString:result.line
                                   lineNum:result.lineNum
                              numberFormat:numberFormat prefix:@"> "]];
    for (unsigned long i=0; i < [result.linesAfter count]; i++) {
        [s appendString:[self formatLineString:result.linesAfter[i]
                                       lineNum:result.lineNum + i + 1
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
