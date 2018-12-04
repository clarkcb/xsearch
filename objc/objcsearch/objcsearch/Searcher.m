#import "common.h"
#import "FileUtil.h"
#import "Searcher.h"
#import "SearchFile.h"

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
    return (([inExtensions count] == 0 || [inExtensions containsObject:ext]) && ([outExtensions count] == 0 || ![outExtensions containsObject:ext]));
}

- (BOOL) isSearchDir:(NSString*)dirPath {
    if ([FileUtil isHidden:dirPath] && self.settings.excludeHidden) {
        return false;
    }
    return [self filterByPatterns:dirPath inPatterns:self.settings.inDirPatterns outPatterns:self.settings.outDirPatterns];
}

- (NSArray<NSString*>*) getSearchDirs:(NSString*)filePath {
    NSMutableArray *searchDirs = [NSMutableArray array];
    NSDirectoryEnumerator *enumerator = [FileUtil enumeratorForPath:filePath];
    NSString *element = (NSString*)[enumerator nextObject];
    while (element != nil) {
        NSString *fullPath = [FileUtil joinPath:filePath childPath:element];
        if ([FileUtil isDirectory:fullPath] && [FileUtil isReadableFile:fullPath] &&
            [self isSearchDir:fullPath]) {
            [searchDirs addObject:fullPath];
        }
        element = (NSString*)[enumerator nextObject];
    }
    return [NSArray arrayWithArray:searchDirs];
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

- (NSArray<SearchFile*>*) getSearchFiles:(NSArray<NSString*>*)searchDirs error:(NSError**)error {
    NSMutableArray<SearchFile*>* searchFiles = [NSMutableArray array];
    for (NSString *d in searchDirs) {
        NSArray *dirFiles = [self getSearchFilesForDirectory:d error:error];
        if (*error != nil) {
            return [NSArray array];
        }
        [searchFiles addObjectsFromArray:dirFiles];
    }
    return [searchFiles sortedArrayUsingComparator:^NSComparisonResult(SearchFile *sf1, SearchFile *sf2) {
        return [[sf1.filePath uppercaseString] compare:[sf2.filePath uppercaseString]];
    }];
}

- (NSArray<SearchFile*>*) getSearchFilesForDirectory:(NSString*)dirPath error:(NSError**)error {
    NSArray<NSString*> *fileNames = [FileUtil contentsForPath:dirPath error:error];
    if (*error != nil) {
        return [NSArray array];
    }

    NSArray<NSString*> *sortedFileNames = [fileNames sortedArrayUsingSelector:@selector(localizedCaseInsensitiveCompare:)];

    NSMutableArray<SearchFile*> *searchFiles = [NSMutableArray array];
    for (NSString *f in sortedFileNames) {
        NSString *fullPath = [FileUtil joinPath:dirPath childPath:f];
        if (![FileUtil isDirectory:fullPath] && [FileUtil isReadableFile:fullPath]
            && [self filterFile:f]) {
            FileType fileType = [self.fileTypes getFileType:f];
            [searchFiles addObject:[[SearchFile alloc]
                                    initWithFilePath:fullPath
                                    fileType:fileType]];
        }
    }
    return [NSArray arrayWithArray:searchFiles];
}

- (NSArray<SearchResult*>*) search:(NSError**)error {
    //logMsg(@"Searching...");
    NSMutableArray<SearchResult*> *results = [NSMutableArray array];
    if ([FileUtil isDirectory:self.settings.startPath]) {
        [results addObjectsFromArray:[self searchDirPath:self.settings.startPath error:error]];
    } else if ([FileUtil isDirectory:[FileUtil expandPath:self.settings.startPath]]) {
        [results addObjectsFromArray:[self searchDirPath:[FileUtil expandPath:self.settings.startPath] error:error]];
    } else {
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
    NSMutableArray<NSString*>* searchDirs = [NSMutableArray arrayWithObjects:filePath, nil];
    if (self.settings.recursive) {
        [searchDirs addObjectsFromArray:[self getSearchDirs:filePath]];
    }
    if (self.settings.verbose) {
        logMsg([NSString stringWithFormat:@"\nDirectories to be searched (%lu):", [searchDirs count]]);
        for (NSString *d in searchDirs) {
            logMsg(d);
        }
    }

    NSArray<SearchFile*> *searchFiles = [self getSearchFiles:searchDirs error:error];
    
    if (self.settings.verbose) {
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
    //logMsg([NSString stringWithFormat:@"Searching text file contents %@", [sf description]]);
    NSString *contents = [[NSString alloc] initWithContentsOfFile:sf.filePath encoding:NSUTF8StringEncoding error:error];
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
            NSString *lineAfter = [self lineFromIndices:s startLineIndex:startLineIndex endLineIndex:endLineIndex];
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
    NSArray<NSTextCheckingResult*> *matches = [pattern matches:s];
    if ([matches count] > 0 && self.settings.firstMatch) {
        matches = [NSArray arrayWithObject:matches[0]];
    }
    for (NSTextCheckingResult *m in matches) {
        int beforeLineCount = [self countLessThan:m.range.location array:startLineIndices] - 1;
        
        NSMutableArray *linesBefore = [NSMutableArray array];
        if (self.settings.linesBefore > 0) {
            int getLineCount = MIN(self.settings.linesBefore, beforeLineCount);
            for (int i=getLineCount; i > 0; i--) {
                long startLineIndex = [startLineIndices[beforeLineCount - i] longValue];
                long endLineIndex = [endLineIndices[beforeLineCount - i] longValue];
                NSString *lineBefore = [self lineFromIndices:s startLineIndex:startLineIndex endLineIndex:endLineIndex];
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
