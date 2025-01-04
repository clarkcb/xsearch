//#import <objcfind/common.h>
#import "common.h"
#import "FileUtil.h"
#import "Regex.h"
#import "Searcher.h"

@implementation Searcher

- (instancetype) initWithSettings:(SearchSettings*)settings error:(NSError**)error {
    self = [super init];
    if (self) {
        self.finder = [[Finder alloc] initWithSettings:settings error:error];
        self.settings = settings;
        if (![self validateSettings:settings error:error]) {
            return self;
        }
    }
    return self;
}

- (NSStringEncoding) strToEncoding:(NSString*)s {
    NSStringEncoding encoding =
        CFStringConvertEncodingToNSStringEncoding(CFStringConvertIANACharSetNameToEncoding((CFStringRef) s));
    return encoding;
}

- (BOOL) validateSettings:(SearchSettings*)settings error:(NSError**)error {
    if (settings == nil) {
        setError(error, @"Settings not defined");
        return false;
    } else if ([settings.searchPatterns count] == 0) {
        setError(error, @"No search patterns defined");
        return false;
    } else if (settings.linesAfter < 0) {
        setError(error, @"Invalid linesafter");
        return false;
    } else if (settings.linesBefore < 0) {
        setError(error, @"Invalid linesbefore");
        return false;
    } else if (settings.maxLineLength < 0) {
        setError(error, @"Invalid maxlinelength");
        return false;
    } else {
        NSStringEncoding encoding = [self strToEncoding:settings.textFileEncoding];
        if (encoding == 0xFFFFFFFF) {
            setError(error, @"Invalid textfileencoding");
            return false;
        } else {
            self.textFileEncoding = encoding;
        }
    }
    return true;
}

- (BOOL) matchesAnyPattern:(NSString*)s patterns:(NSArray<Regex*>*)patterns {
    for (Regex *r in patterns) {
        if ([r test:s]) {
            return true;
        }
    }
    return false;
}

- (BOOL) anyMatchesAnyPattern:(NSArray<NSString*>*)ss patterns:(NSArray<Regex*>*)patterns {
    for (NSString *s in ss) {
        if ([self matchesAnyPattern:s patterns:patterns]) {
            return true;
        }
    }
    return false;
}

- (BOOL) filterByExtensions:(NSString*)ext inExtensions:(NSArray<NSString*>*)inExtensions outExtensions:(NSArray<NSString*>*)outExtensions {
    return (([inExtensions count] == 0 || [inExtensions containsObject:ext]) &&
            ([outExtensions count] == 0 || ![outExtensions containsObject:ext]));
}

- (BOOL) filterByPatterns:(NSString*)s inPatterns:(NSArray<Regex*>*)inPatterns outPatterns:(NSArray<Regex*>*)outPatterns {
    return (([inPatterns count] == 0 || [self matchesAnyPattern:s patterns:inPatterns]) &&
            ([outPatterns count] == 0 || ![self matchesAnyPattern:s patterns:outPatterns]));
}

- (BOOL) filterByTypes:(FileType)fileType inTypes:(NSArray<NSNumber*>*)inTypes outTypes:(NSArray<NSNumber*>*)outTypes {
    NSNumber *num = [NSNumber numberWithInt:fileType];
    return (([inTypes count] == 0 || [inTypes containsObject:num]) &&
            ([outTypes count] == 0 || ![outTypes containsObject:num]));
}

- (NSArray<SearchResult*>*) search:(NSError**)error {
    //logMsg(@"Searching...");
    NSMutableArray<SearchResult*> *results = [NSMutableArray array];
    NSArray<FileResult*> *fileResults = [self.finder find:error];
    for (FileResult *fr in fileResults) {
        [results addObjectsFromArray:[self searchFile:fr error:error]];
    }
    return [NSArray arrayWithArray:results];
}

- (NSArray<SearchResult*>*) searchFile:(FileResult*)fr error:(NSError**)error {
    switch (fr.fileType) {
        case FileTypeCode:
        case FileTypeText:
        case FileTypeXml:
            return [self searchTextFile:fr error:error];
            break;
        case FileTypeBinary:
            return [self searchBinaryFile:fr error:error];
            break;
        case FileTypeArchive:
        default:
            return [NSArray array];
            break;
    }
}

- (NSArray<SearchResult*>*) searchBinaryFile:(FileResult*)fr error:(NSError**)error {
    NSString *contents = [[NSString alloc] initWithContentsOfFile:fr.filePath
                                                         encoding:NSISOLatin1StringEncoding
                                                            error:error];
    if (*error != nil) {
        return [NSArray array];
    }
    NSMutableArray<SearchResult*> *results = [NSMutableArray array];
    if (contents != nil) {
        for (Regex *p in self.settings.searchPatterns) {
            NSArray<NSTextCheckingResult*> *matches = [p matches:contents];
            if ([matches count] > 0 && self.settings.firstMatch) {
                matches = [NSArray arrayWithObject:matches[0]];
            }
            for (NSTextCheckingResult *m in matches) {
                SearchResult *r = [[SearchResult alloc] initWithPattern:p.pattern
                                                                   file:fr
                                                                lineNum:0
                                                        matchStartIndex:m.range.location + 1
                                                          matchEndIndex:m.range.location + m.range.length + 1
                                                                   line:@""
                                                            linesBefore:[NSArray array]
                                                             linesAfter:[NSArray array]];
                [results addObject:r];
            }
        }
    }
    return results;
}

- (NSArray<SearchResult*>*) searchTextFile:(FileResult*)fr error:(NSError**)error {
    if (self.settings.multiLineSearch) {
        return [self searchTextFileContents:fr error:error];
    } else {
        // there is no line reader in ObjC, might not bother
        // implementing line-based searching
        //[self searchTextFileLines:sf error:error];
        return [self searchTextFileContents:fr error:error];
    }
}

- (NSArray<SearchResult*>*) searchTextFileContents:(FileResult*)fr error:(NSError**)error {
    NSString *contents = [[NSString alloc] initWithContentsOfFile:fr.filePath
                                                         encoding:self.textFileEncoding
                                                            error:error];
    if (*error != nil) {
        if ([[*error domain] isEqualToString:@"NSCocoaErrorDomain"] && [*error code] == 261) {
            // this indicates problem reading text file with encoding, just reset the error and move on
            *error = nil;
        } else {
            return [NSArray array];
        }
    }
    NSArray<SearchResult*> *results = [NSArray array];
    if (contents != nil) {
        results = [self searchMultiLineString:contents error:error];
        for (SearchResult *r in results) {
            r.file = fr;
        }
    }
    return results;
}

- (NSArray<SearchResult*>*) searchMultiLineString:(NSString*)s error:(NSError**)error {
    NSMutableArray<SearchResult*> *results = [NSMutableArray array];
    for (Regex *p in self.settings.searchPatterns) {
        [results addObjectsFromArray:[self searchMultiLineString:s pattern:p error:error]];
    }
    return [NSArray arrayWithArray:results];
}



- (NSArray<NSString*>*) getLinesAfter:(NSString*)s
                      beforeLineCount:(int)beforeLineCount
                     startLineIndices:(NSArray<NSNumber*>*)startLineIndices
                       endLineIndices:(NSArray<NSNumber*>*)endLineIndices {
    NSMutableArray *linesAfter = [NSMutableArray array];
    
    if (self.settings.linesAfter > 0) {
        int afterLineCount = (int)[startLineIndices count] - beforeLineCount - 1;
        int getLineCount = MIN(self.settings.linesAfter, afterLineCount);
        for (int i=1; i <= getLineCount; i++) {
            long startLineIndex = [startLineIndices[beforeLineCount + i] longValue];
            long endLineIndex = [endLineIndices[beforeLineCount + i] longValue];
            NSString *lineAfter = [self lineFromIndices:s
                                         startLineIndex:startLineIndex
                                           endLineIndex:endLineIndex];
            [linesAfter addObject:lineAfter];
        }
    }
    
    return [NSArray arrayWithArray:linesAfter];
}

- (NSArray<SearchResult*>*) searchMultiLineString:(NSString*)s
                                          pattern:(Regex*)pattern
                                            error:(NSError**)error {
    NSMutableArray<SearchResult*> *results = [NSMutableArray array];
    NSArray<NSNumber*> *newLineIndices = [self getNewLineIndices:s];
    NSArray<NSNumber*> *startLineIndices = [self getStartLineIndices:newLineIndices];
    NSArray<NSNumber*> *endLineIndices = [self getEndLineIndices:newLineIndices lastIndex:[s length]];
    NSArray<NSTextCheckingResult*> *matches;
    if (self.settings.firstMatch) {
        NSTextCheckingResult *match = [pattern firstMatch:s];
        if (match != nil) {
            matches = [NSArray arrayWithObject:match];
        } else {
            matches = [NSArray array];
        }
    } else {
        matches = [pattern matches:s];
    }
    for (NSTextCheckingResult *m in matches) {
        int beforeLineCount = [self countLessThan:m.range.location array:startLineIndices] - 1;
        if (beforeLineCount < 0) {
            beforeLineCount = 0;
        }
        NSMutableArray *linesBefore = [NSMutableArray array];
        if (self.settings.linesBefore > 0) {
            int getLineCount = MIN(self.settings.linesBefore, beforeLineCount);
            for (int i=getLineCount; i > 0; i--) {
                long startLineIndex = [startLineIndices[beforeLineCount - i] longValue];
                long endLineIndex = [endLineIndices[beforeLineCount - i] longValue];
                NSString *lineBefore = [self lineFromIndices:s
                                              startLineIndex:startLineIndex
                                                endLineIndex:endLineIndex];
                [linesBefore addObject:lineBefore];
            }
        }

        NSArray *linesAfter = [NSArray array];
        if (self.settings.linesAfter > 0) {
            linesAfter = [self getLinesAfter:s
                             beforeLineCount:beforeLineCount
                            startLineIndices:startLineIndices
                              endLineIndices:endLineIndices];
        }

        if (([linesBefore count] == 0 || [self linesBeforeMatch:linesBefore]) &&
            ([linesAfter count] == 0 || [self linesAfterMatch:linesAfter])) {
            long startLineIndex = [startLineIndices[beforeLineCount] longValue];
            long endLineIndex = [endLineIndices[beforeLineCount] longValue];
            NSString *line = [self lineFromIndices:s startLineIndex:startLineIndex endLineIndex:endLineIndex];
            long lineNum = beforeLineCount + 1;
            long matchStartIndex = m.range.location - startLineIndex + 1;
            long matchEndIndex = m.range.location + m.range.length - startLineIndex + 1;
            SearchResult *r = [[SearchResult alloc] initWithPattern:pattern.pattern
                                                               file:nil
                                                            lineNum:lineNum
                                                    matchStartIndex:matchStartIndex
                                                      matchEndIndex:matchEndIndex
                                                               line:line
                                                        linesBefore:[NSArray arrayWithArray:linesBefore]
                                                         linesAfter:linesAfter];
            [results addObject:r];
        }
    }
    return [NSArray arrayWithArray:results];
}

- (BOOL) linesMatch:(NSArray<NSString*>*)lines
         inPatterns:(NSArray<Regex*>*)inPatterns
        outPatterns:(NSArray<Regex*>*)outPatterns {
    return (([inPatterns count] == 0 || [self anyMatchesAnyPattern:lines patterns:inPatterns]) &&
            ([outPatterns count] == 0 || ![self anyMatchesAnyPattern:lines patterns:outPatterns]));
}

- (BOOL) linesBeforeMatch:(NSArray<NSString*>*)linesBefore {
    return [self linesMatch:linesBefore
                 inPatterns:self.settings.inLinesBeforePatterns
                outPatterns:self.settings.outLinesBeforePatterns];
}

- (BOOL) linesAfterMatch:(NSArray<NSString*>*)linesAfter {
    return [self linesMatch:linesAfter
                 inPatterns:self.settings.inLinesAfterPatterns
                outPatterns:self.settings.outLinesAfterPatterns];
}

- (NSArray<NSNumber*>*) getNewLineIndices:(NSString*)s {
    NSMutableArray<NSNumber*> *newLineIndices = [NSMutableArray array];
    for (long i=0; i < [s length]; i++) {
        unichar c = [s characterAtIndex:i];
        if (c == '\n') {
            [newLineIndices addObject:[NSNumber numberWithLong:i]];
        }
    }
    return [NSArray arrayWithArray:newLineIndices];
}

- (NSArray<NSNumber*>*) getStartLineIndices:(NSArray<NSNumber*>*)newLineIndices {
    NSMutableArray<NSNumber*> *startLineIndices = [NSMutableArray arrayWithObject:[NSNumber numberWithLong:0]];
    for (NSNumber *n in newLineIndices) {
        [startLineIndices addObject:[NSNumber numberWithInteger:[n longValue] + 1]];
    }
    return [NSArray arrayWithArray:startLineIndices];
}

- (NSArray<NSNumber*>*) getEndLineIndices:(NSArray<NSNumber*>*)newLineIndices lastIndex:(long)lastIndex {
    NSMutableArray<NSNumber*> *endLineIndices = [NSMutableArray arrayWithArray:newLineIndices];
    [endLineIndices addObject:[NSNumber numberWithLong:lastIndex]];
    return [NSArray arrayWithArray:endLineIndices];
}

- (int) countLessThan:(long)num array:(NSArray<NSNumber*>*)array {
    int count = 0;
    while ( count < [array count] && [array[count] longValue] < num) {
        count++;
    }
    return count;
}

- (NSString*) lineFromIndices:(NSString*)s
               startLineIndex:(long)startLineIndex
                 endLineIndex:(long)endLineIndex {
    NSString *line = [s substringWithRange:NSMakeRange(startLineIndex, endLineIndex - startLineIndex)];
    return line;
}

@end
