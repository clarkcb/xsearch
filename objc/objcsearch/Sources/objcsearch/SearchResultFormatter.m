#import "FileUtil.h"
#import "SearchResultFormatter.h"

@implementation SearchResultFormatter

- (instancetype) initWithSettings:(SearchSettings*)settings
{
    self = [super init];
    if (self) {
        // necessary to reference other methods in self from constructor
        __weak typeof(self) weakSelf = self;

        self.settings = settings;
        self.fileFormatter = [[FileResultFormatter alloc] initWithSettings:settings];

        if (settings.colorize) {
            self.formatLine = ^( NSString* line) { return [weakSelf formatLineWithColor:line]; };
            self.formatMatch = ^( NSString* match) { return [weakSelf formatMatchWithColor:match]; };
        } else {
            self.formatLine = ^( NSString* line) { return line; };
            self.formatMatch = ^( NSString* match) { return match; };
        }
    }
    return self;
}

- (NSString *) formatLineWithColor:(NSString*)line {
    NSString *formattedLine = line;
    for (Regex *p in self.settings.searchPatterns) {
        NSTextCheckingResult *m = [p firstMatch:formattedLine];
        if (m != nil) {
            formattedLine = [self colorize:formattedLine matchStartIndex:m.range.location matchEndIndex:m.range.location + m.range.length color:[self.settings lineColor]];
            break;
        }
    }
    return formattedLine;
}

- (NSString *) formatMatchWithColor:(NSString*)match {
    long matchLen = [match length];
    return [self colorize:match matchStartIndex:0L matchEndIndex:matchLen color:[self.settings lineColor]];
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
    [s appendString:[self.fileFormatter formatFileResult:result.file]];
    if (result.lineNum > 0) {
        [s appendFormat:@": %lu: [%lu:%lu]: ", result.lineNum,
         result.matchStartIndex, result.matchEndIndex];
        [s appendString:[self formatMatchingLine:result]];
    } else {
        [s appendFormat:@" matches at [%lu:%lu]",
         result.matchStartIndex, result.matchEndIndex];
    }
    return [NSString stringWithString:s];
}

- (NSString *) colorize:(NSString*)s matchStartIndex:(long)matchStartIndex matchEndIndex:(long)matchEndIndex color:(Color)color {
    return [self.fileFormatter colorize:s matchStartIndex:matchStartIndex matchEndIndex:matchEndIndex color:color];
}

- (NSString *) formatLineMatch:(SearchResult*)result {
    if (result.line == nil || [result.line isEqualToString:@""] || _settings.maxLineLength == 0) {
        return @"";
    }
    
    long matchStartIndex = result.matchStartIndex - 1;
    long matchEndIndex = result.matchEndIndex - 1;
    long matchLength = result.matchEndIndex - result.matchStartIndex;

    NSMutableString *prefix = [NSMutableString stringWithString:@""];
    NSMutableString *suffix = [NSMutableString stringWithString:@""];
    long colorStartIndex = 0;
    long colorEndIndex = matchLength;

    if (matchLength > _settings.maxLineLength) {
        if (matchStartIndex > 2) {
            [prefix appendString:@"..."];
        }
        [suffix appendString:@"..."];
        colorStartIndex = [prefix length];
        colorEndIndex = _settings.maxLineLength - [suffix length];
        matchEndIndex = matchStartIndex + colorEndIndex;
        matchStartIndex = matchStartIndex + colorStartIndex;
    }
    
    NSMutableString *matchString = [NSMutableString stringWithFormat:@"%@%@%@", prefix, [result.line substringWithRange:NSMakeRange(matchStartIndex, matchEndIndex - matchStartIndex)], suffix];

    if (self.settings.colorize) {
        return [self colorize:matchString matchStartIndex:colorStartIndex matchEndIndex:colorEndIndex color:self.settings.lineColor];
    }

    return [NSString stringWithString:matchString];
}

- (NSString *) formatMatchingLine:(SearchResult*)result {
    if (result.line == nil || [result.line isEqualToString:@""] || _settings.maxLineLength == 0) {
        return @"";
    }
    
    BOOL maxLimit = _settings.maxLineLength > 0;

    if (maxLimit && result.matchEndIndex - result.matchStartIndex > _settings.maxLineLength) {
        return [self formatLineMatch:result];
    }

    long lineStartIndex = 0;
    long lineEndIndex = [result.line length] - 1;
    NSCharacterSet *whitespaceCharacterSet = [NSCharacterSet characterSetWithCharactersInString:@" \t\r\n"];
    while ([whitespaceCharacterSet characterIsMember:[result.line characterAtIndex:lineStartIndex]]) {
        lineStartIndex++;
    }
    while ([whitespaceCharacterSet characterIsMember:[result.line characterAtIndex:lineEndIndex]]) {
        lineEndIndex--;
    }

    NSMutableString *trimmed = [NSMutableString string];
    [trimmed appendString:[result.line stringByTrimmingCharactersInSet:whitespaceCharacterSet]];
    long trimmedLength = [trimmed length];

    long matchLength = result.matchEndIndex - result.matchStartIndex;
    long matchStartIndex = result.matchStartIndex - 1 - lineStartIndex;
    long matchEndIndex = matchStartIndex + matchLength;

    NSMutableString *prefix = [NSMutableString stringWithString:@""];
    NSMutableString *suffix = [NSMutableString stringWithString:@""];

    if (maxLimit && trimmedLength > _settings.maxLineLength) {
        lineStartIndex = result.matchStartIndex - 1;
        lineEndIndex = lineStartIndex + matchLength;
        matchStartIndex = 0;
        matchEndIndex = matchLength;

        long currentLen = lineEndIndex - lineStartIndex;
        while (currentLen < _settings.maxLineLength) {
            if (lineStartIndex > 0) {
                lineStartIndex--;
                matchStartIndex++;
                matchEndIndex++;
                currentLen++;
            }
            if (currentLen < _settings.maxLineLength && lineEndIndex < trimmedLength) {
                lineEndIndex++;
                currentLen++;
            }
        }
        
        if (lineStartIndex > 2) {
            [prefix appendString:@"..."];
            lineStartIndex += 3;
        }
        if (lineEndIndex < trimmedLength - 3) {
            [suffix appendString:@"..."];
            lineEndIndex -= 3;
        }
    } else {
        lineEndIndex += 1;
    }


    NSMutableString *formatted = [NSMutableString stringWithFormat:@"%@%@%@", prefix, [result.line substringWithRange:NSMakeRange(lineStartIndex, lineEndIndex - lineStartIndex)], suffix];

    if (self.settings.colorize) {
        return [self colorize:formatted matchStartIndex:matchStartIndex matchEndIndex:matchEndIndex color:self.settings.lineColor];
    }

    return [NSString stringWithString:formatted];
}

- (NSString *) multiLineFormat:(SearchResult*)result {
    NSMutableString *s = [NSMutableString string];
    int sepLen = 80;
    [s appendFormat:@"%@\n", [self getRepeatingString:@"=" count:sepLen]];
    [s appendString:[self.fileFormatter formatFileResult:result.file]];
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
    if ([line hasSuffix:@"\n"]) {
        [s appendFormat:@"%@ | %@", numberString, line];
    } else {
        [s appendFormat:@"%@ | %@\n", numberString, line];
    }
    return [NSString stringWithString:s];
}

@end
