#import "common.h"
#import "FileUtil.h"
#import "Searcher.h"

@implementation Searcher

- (instancetype) initWithSettings:(SearchSettings*)settings error:(NSError**)error {
    self = [super init];
    if (self) {
        self.fileTypes = [[FileTypes alloc] init];
        self.settings = settings;
        [self validateSettings:settings error:error];
    }
    return self;
}

- (NSStringEncoding) strToEncoding:(NSString*)s {
    s = [s lowercaseString];
    NSStringEncoding encoding = 0;
    if ([s isEqualToString:@"utf-8"] || [s isEqualToString:@"utf8"]) {
        encoding = NSUTF8StringEncoding;
    } else if ([s isEqualToString:@"utf-16"] || [s isEqualToString:@"utf16"]) {
        encoding = NSUTF16StringEncoding;
    } else if ([s isEqualToString:@"utf-32"] || [s isEqualToString:@"utf32"]) {
        encoding = NSUTF32StringEncoding;
    } else if ([s isEqualToString:@"iso-8859-1"] || [s isEqualToString:@"iso88591"]
               || [s isEqualToString:@"iso-latin-1"] || [s isEqualToString:@"isolatin1"]
               || [s isEqualToString:@"latin-1"] || [s isEqualToString:@"latin1"]) {
        encoding = NSISOLatin1StringEncoding;
    } else if ([s isEqualToString:@"macosroman"]) {
        encoding = NSMacOSRomanStringEncoding;
    } else if ([s isEqualToString:@"windows-1252"] || [s isEqualToString:@"windows1252"]
               || [s isEqualToString:@"cp-1252"] || [s isEqualToString:@"cp1252"]) {
        encoding = NSWindowsCP1252StringEncoding;
    } else if ([s isEqualToString:@"shift-jis"] || [s isEqualToString:@"shiftjis"]) {
        encoding = NSShiftJISStringEncoding;
    } else if ([s isEqualToString:@"ascii"]) {
        encoding = NSASCIIStringEncoding;
    }
    return encoding;
}

- (void) validateSettings:(SearchSettings*)settings error:(NSError**)error {
    if (settings == nil) {
        setError(error, @"Settings not defined");
    } else if (settings.startPath == nil || [settings.startPath length] == 0) {
        setError(error, @"Startpath not defined");
    } else if (![FileUtil exists:settings.startPath] && ![FileUtil exists:[FileUtil expandPath:settings.startPath]]) {
        setError(error, @"Startpath not found");
    } else if (![FileUtil isReadableFile:settings.startPath] && ![FileUtil isReadableFile:[FileUtil expandPath:settings.startPath]]) {
        setError(error, @"Startpath not readable");
    } else if ([settings.searchPatterns count] == 0) {
        setError(error, @"No search patterns defined");
    } else if (settings.linesAfter < 0) {
        setError(error, @"Invalid linesafter");
    } else if (settings.linesBefore < 0) {
        setError(error, @"Invalid linesbefore");
    } else if (settings.maxLineLength < 0) {
        setError(error, @"Invalid maxlinelength");
    } else {
        NSStringEncoding encoding = [self strToEncoding:settings.textFileEncoding];
        if (encoding == 0) {
            setError(error, @"Invalid textfileencoding");
        } else {
            self.textFileEncoding = encoding;
        }
    }
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

- (BOOL) filterByPatterns:(NSString*)s inPatterns:(NSArray<Regex*>*)inPatterns outPatterns:(NSArray<Regex*>*)outPatterns {
    return (([inPatterns count] == 0 || [self matchesAnyPattern:s patterns:inPatterns]) && ([outPatterns count] == 0 || ![self matchesAnyPattern:s patterns:outPatterns]));
}

- (BOOL) filterByExtensions:(NSString*)ext inExtensions:(NSArray<NSString*>*)inExtensions outExtensions:(NSArray<NSString*>*)outExtensions {
    return (([inExtensions count] == 0 || [inExtensions containsObject:ext])
            && ([outExtensions count] == 0 || ![outExtensions containsObject:ext]));
}

- (BOOL) isSearchDir:(NSString*)dirPath {
    if ([FileUtil isHidden:dirPath] && self.settings.excludeHidden) {
        return false;
    }
    return [self filterByPatterns:dirPath inPatterns:self.settings.inDirPatterns outPatterns:self.settings.outDirPatterns];
}

- (BOOL) isSearchFile:(NSString*)filePath {
    return [self filterByExtensions:[FileUtil getExtension:filePath]
                       inExtensions:self.settings.inExtensions
                      outExtensions:self.settings.outExtensions] &&
    [self filterByPatterns:filePath
                inPatterns:self.settings.inFilePatterns
               outPatterns:self.settings.outFilePatterns];
}

- (BOOL) isArchiveSearchFile:(NSString*)filePath {
    return [self filterByExtensions:[FileUtil getExtension:filePath]
                       inExtensions:self.settings.inArchiveExtensions
                      outExtensions:self.settings.outArchiveExtensions] &&
    [self filterByPatterns:filePath
                inPatterns:self.settings.inArchiveFilePatterns
               outPatterns:self.settings.outArchiveFilePatterns];
}

- (BOOL) filterFile:(NSString*)filePath {
    if ([FileUtil isHidden:filePath] && self.settings.excludeHidden) {
        return false;
    }
    FileType fileType = [self.fileTypes getFileType:filePath];
    //if (fileType == FileTypeUnknown) {
    //    return false;
    //}
    if (fileType == FileTypeArchive) {
        return self.settings.searchArchives && [self isArchiveSearchFile:filePath];
    }
    return !self.settings.archivesOnly && [self isSearchFile:filePath];
}

- (SearchFile*) filterToSearchFile:(NSString*)filePath {
    if ([FileUtil isHidden:filePath] && self.settings.excludeHidden) {
        return false;
    }
    FileType fileType = [self.fileTypes getFileType:filePath];
    if (fileType == FileTypeUnknown) {
        return nil;
    }
    if ((fileType == FileTypeArchive && self.settings.searchArchives && [self isArchiveSearchFile:filePath]) ||
        (!self.settings.archivesOnly && [self isSearchFile:filePath])) {
        return [[SearchFile alloc]
                initWithFilePath:filePath
                fileType:fileType];
    }
    return nil;
}

- (NSArray<SearchResult*>*) search:(NSError**)error {
    //logMsg(@"Searching...");
    NSMutableArray<SearchResult*> *results = [NSMutableArray array];
    if ([FileUtil isDirectory:self.settings.startPath]) {
        [results addObjectsFromArray:[self searchDirPath:self.settings.startPath error:error]];
    } else {
        // TODO: looks like we need to do filterFile on this
        FileType fileType = [self.fileTypes getFileType:self.settings.startPath];
        SearchFile *sf = [[SearchFile alloc]
                                initWithFilePath:self.settings.startPath
                                fileType:fileType];
        [results addObjectsFromArray:[self searchFile:sf error:error]];
    }
    return [NSArray arrayWithArray:results];
}

- (NSArray<SearchResult*>*) searchDirPath:(NSString*)filePath error:(NSError**)error {
    //logMsg(@"Searching path...");
    NSArray<SearchFile*> *searchFiles = [self getSearchFiles:filePath];

    if (self.settings.verbose) {
        NSMutableSet<NSString*> *dirSet = [NSMutableSet set];
        for (SearchFile *sf in searchFiles) {
            [dirSet addObject:[[sf description] stringByDeletingLastPathComponent]];
        }
        NSArray *searchDirs = [NSArray arrayWithArray:[dirSet allObjects]];
        searchDirs = [searchDirs sortedArrayUsingComparator:^NSComparisonResult(NSString *s1, NSString *s2) {
            return [s1 compare:s2];
        }];

        logMsg([NSString stringWithFormat:@"\nDirectories to be searched (%lu):", [searchDirs count]]);
        for (NSString *d in searchDirs) {
            logMsg(d);
        }

        logMsg([NSString stringWithFormat:@"\nFiles to be searched (%lu):", [searchFiles count]]);
        for (SearchFile *sf in searchFiles) {
            logMsg([sf description]);
        }
    }

    NSMutableArray<SearchResult*> *results = [NSMutableArray array];
    for (SearchFile *sf in searchFiles) {
        [results addObjectsFromArray:[self searchFile:sf error:error]];
        if (*error != nil) {
            return [NSArray array];
        }
    }
    return [NSArray arrayWithArray:results];
}

- (NSArray<SearchFile*>*) getSearchFiles:(NSString*)filePath {
    NSMutableArray *searchFiles = [NSMutableArray array];
    NSDirectoryEnumerator *enumerator = [FileUtil enumeratorForPath:filePath settings:self.settings];
    NSURL *element = (NSURL*)[enumerator nextObject];
    while (element != nil) {
        NSNumber *isDirectory = nil;
        [element getResourceValue:&isDirectory forKey:NSURLIsDirectoryKey error:nil];
        if ([isDirectory boolValue]) {
            if (![self isSearchDir:[element path]]) {
                [enumerator skipDescendants];
            }
        } else {
            NSNumber *isRegularFile = nil;
            [element getResourceValue:&isRegularFile forKey:NSURLIsRegularFileKey error:nil];
            if ([isRegularFile boolValue]) {
                NSString *filePath = [element path];
                SearchFile *searchFile = [self filterToSearchFile:filePath];
                if (searchFile != nil) {
                    [searchFiles addObject:searchFile];
                }
            }
        }
        element = (NSURL*)[enumerator nextObject];
    }
    return [NSArray arrayWithArray:searchFiles];
}


- (NSArray<SearchResult*>*) searchFilePath:(NSString*)filePath error:(NSError**)error {
    FileType fileType = [self.fileTypes getFileType:filePath];
    SearchFile *sf = [[SearchFile alloc] initWithFilePath:filePath fileType:fileType];
    return [self searchFile:sf error:error];
}

- (NSArray<SearchResult*>*) searchFile:(SearchFile*)sf error:(NSError**)error {
    switch (sf.fileType) {
        case FileTypeCode:
        case FileTypeText:
        case FileTypeXml:
            return [self searchTextFile:sf error:error];
            break;
        case FileTypeBinary:
            return [self searchBinaryFile:sf error:error];
            break;
        case FileTypeArchive:
        default:
            return [NSArray array];
            break;
    }
}

- (NSArray<SearchResult*>*) searchBinaryFile:(SearchFile*)sf error:(NSError**)error {
    NSString *contents = [[NSString alloc] initWithContentsOfFile:sf.filePath encoding:NSISOLatin1StringEncoding error:error];
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
                                                                   file:sf
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

- (NSArray<SearchResult*>*) searchTextFile:(SearchFile*)sf error:(NSError**)error {
    NSArray<SearchResult*> *results = [NSArray array];
    if (self.settings.multiLineSearch) {
        results = [self searchTextFileContents:sf error:error];
    } else {
        // there is no line reader in ObjC, might not bother
        // implementing line-based searching
        //[self searchTextFileLines:sf error:error];
        results = [self searchTextFileContents:sf error:error];
    }
    return results;
}

- (NSArray<SearchResult*>*) searchTextFileContents:(SearchFile*)sf error:(NSError**)error {
    NSString *contents = [[NSString alloc] initWithContentsOfFile:sf.filePath
                                                         encoding:self.textFileEncoding
                                                            error:error];
    if (*error != nil) {
        return [NSArray array];
    }
    NSArray<SearchResult*> *results = [NSArray array];
    if (contents != nil) {
        results = [self searchMultiLineString:contents error:error];
        for (SearchResult *r in results) {
            r.file = sf;
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

- (NSArray<SearchResult*>*) searchMultiLineString:(NSString*)s pattern:(Regex*)pattern error:(NSError**)error {
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
