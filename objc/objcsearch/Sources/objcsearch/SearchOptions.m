#import "ArgTokenizer.h"
#import "SearchConfig.h"
#import "Regex.h"
#import "SearchOptions.h"

@interface SearchOptions ()
// private properties
@property NSArray<SearchOption*> *searchOptions;
@property NSDictionary<NSString*,NSString*> *longArgDict;
@property NSDictionary *boolActionDict;
@property NSDictionary *stringActionDict;
@property NSDictionary *integerActionDict;
@property ArgTokenizer *argTokenizer;

@end

@implementation SearchOptions

- (instancetype) init {
    self = [super init];
    if (self) {
        self.longArgDict = [self getLongArgDict];
        self.boolActionDict = [self getBoolActionDict];
        self.stringActionDict = [self getStringActionDict];
        self.integerActionDict = [self getIntegerActionDict];
        self.searchOptions = [self searchOptionsFromJson];
        self.argTokenizer = [[ArgTokenizer alloc] initWithOptions:self.searchOptions];
    }
    return self;
}

- (NSArray<SearchOption*>*) searchOptionsFromJson {
    NSMutableString *searchOptionsJsonPath = [NSMutableString stringWithString:getXsearchSharedPath()];
    [searchOptionsJsonPath appendString:@"/searchoptions.json"];
    
    if (![[NSFileManager defaultManager] fileExistsAtPath:searchOptionsJsonPath]) {
        return nil;
    }
    
    NSMutableArray *searchOptions = [[NSMutableArray alloc] initWithCapacity:44];

    NSData *data = [NSData dataWithContentsOfFile:searchOptionsJsonPath];
    
    if (NSClassFromString(@"NSJSONSerialization")) {
        NSError *error = nil;
        id jsonObject = [NSJSONSerialization
                         JSONObjectWithData:data
                         options:0
                         error:&error];
        
        if (error) { /* JSON was malformed, act appropriately here */ }
        
        if ([jsonObject isKindOfClass:[NSDictionary class]]) {
            NSArray *searchOptionObjects = jsonObject[@"searchoptions"];
            for (NSDictionary *searchOptionDict in searchOptionObjects) {
                NSString *lArg = searchOptionDict[@"long"];
                NSString *sArg = searchOptionDict[@"short"];
                NSString *desc = searchOptionDict[@"desc"];
                ArgTokenType argType = ArgTokenTypeUnknown;
                if (self.boolActionDict[lArg]) {
                    argType = ArgTokenTypeBool;
                } else if (self.stringActionDict[lArg] || [lArg isEqualToString:@"settings-file"]) {
                    argType = ArgTokenTypeStr;
                } else if (self.integerActionDict[lArg]) {
                    argType = ArgTokenTypeInt;
                }
                [searchOptions addObject:[[SearchOption alloc] initWithShortArg:sArg withLongArg:lArg withDesc:desc withArgType:argType]];
            }
        }
    }

    return [NSArray arrayWithArray:searchOptions];
}

- (NSDictionary<NSString*,NSString*>*) getLongArgDict {
    NSMutableDictionary *longArgDict = [[NSMutableDictionary alloc] initWithCapacity:70];
    for (SearchOption *so in self.searchOptions) {
        longArgDict[so.longArg] = so.longArg;
        if (so.shortArg) {
            longArgDict[so.shortArg] = so.longArg;
        }
    }
    // Add path here because it isn't included in findoptions.json
    longArgDict[@"path"] = @"path";
    return [NSDictionary dictionaryWithDictionary:longArgDict];
}

typedef void (^BoolActionBlockType)(BOOL, SearchSettings*);

- (NSDictionary<NSString*,BoolActionBlockType>*) getBoolActionDict {
    return @{
        @"allmatches" : [^void (BOOL b, SearchSettings *ss) { ss.firstMatch = !b; } copy],
        @"archivesonly" : [^void (BOOL b, SearchSettings *ss) {
            ss.archivesOnly = b;
            if (b) ss.searchArchives = true;
        } copy],
        @"colorize" : [^void (BOOL b, SearchSettings *ss) { ss.colorize = b; } copy],
        @"debug" : [^void (BOOL b, SearchSettings *ss) {
            ss.debug = b;
            if (b) ss.verbose = true;
        } copy],
        @"excludehidden" : [^void (BOOL b, SearchSettings *ss) { ss.includeHidden = !b; } copy],
        @"firstmatch" : [^void (BOOL b, SearchSettings *ss) { ss.firstMatch = b; } copy],
        @"followsymlinks" : [^void (BOOL b, SearchSettings *ss) { ss.followSymlinks = b; } copy],
        @"help" : [^void (BOOL b, SearchSettings *ss) { ss.printUsage = b; } copy],
        @"includehidden" : [^void (BOOL b, SearchSettings *ss) { ss.includeHidden = b; } copy],
        @"multilinesearch" : [^void (BOOL b, SearchSettings *ss) { ss.multiLineSearch = b; } copy],
        @"nocolorize" : [^void (BOOL b, SearchSettings *ss) { ss.colorize = !b; } copy],
        @"nofollowsymlinks" : [^void (BOOL b, SearchSettings *ss) { ss.followSymlinks = !b; } copy],
        @"noprintdirs" : [^void (BOOL b, SearchSettings *ss) { ss.printDirs = !b; } copy],
        @"noprintfiles" : [^void (BOOL b, SearchSettings *ss) { ss.printFiles = !b; } copy],
        @"noprintlines" : [^void (BOOL b, SearchSettings *ss) { ss.printLines = !b; } copy],
        @"noprintmatches" : [^void (BOOL b, SearchSettings *ss) { ss.printResults = !b; } copy],
        @"norecursive" : [^void (BOOL b, SearchSettings *ss) { ss.recursive = !b; } copy],
        @"nosearcharchives" : [^void (BOOL b, SearchSettings *ss) { ss.searchArchives = !b; } copy],
        @"printdirs" : [^void (BOOL b, SearchSettings *ss) { ss.printDirs = b; } copy],
        @"printfiles" : [^void (BOOL b, SearchSettings *ss) { ss.printFiles = b; } copy],
        @"printlines" : [^void (BOOL b, SearchSettings *ss) { ss.printLines = b; } copy],
        @"printmatches" : [^void (BOOL b, SearchSettings *ss) { ss.printResults = b; } copy],
        @"recursive" : [^void (BOOL b, SearchSettings *ss) { ss.recursive = b; } copy],
        @"searcharchives" : [^void (BOOL b, SearchSettings *ss) { ss.searchArchives = b; } copy],
        @"sort-ascending" : [^void (BOOL b, SearchSettings *ss) { ss.sortDescending = !b; } copy],
        @"sort-caseinsensitive" : [^void (BOOL b, SearchSettings *ss) { ss.sortCaseInsensitive = b; } copy],
        @"sort-casesensitive" : [^void (BOOL b, SearchSettings *ss) { ss.sortCaseInsensitive = !b; } copy],
        @"sort-descending" : [^void (BOOL b, SearchSettings *ss) { ss.sortDescending = b; } copy],
        @"uniquelines" : [^void (BOOL b, SearchSettings *ss) { ss.uniqueLines = b; } copy],
        @"verbose" : [^void (BOOL b, SearchSettings *ss) { ss.verbose = b; } copy],
        @"version" : [^void (BOOL b, SearchSettings *ss) { ss.printVersion = b; } copy]
    };
}

typedef void (^StringActionBlockType)(NSString*, SearchSettings*);

- (NSDictionary<NSString*,StringActionBlockType>*) getStringActionDict {
    return @{
        @"encoding" : ^void (NSString* s, SearchSettings *ss) {
            ss.textFileEncoding = s;
        },
        @"in-archiveext" : ^void (NSString* s, SearchSettings *ss) {
            [ss addInArchiveExtension:s];
        },
        @"in-archivefilepattern" : ^void (NSString* s, SearchSettings *ss) {
            [ss.inArchiveFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"in-dirpattern" : ^void (NSString* s, SearchSettings *ss) {
            [ss.inDirPatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"in-ext" : ^void (NSString* s, SearchSettings *ss) {
            [ss addInExtension:s];
        },
        @"in-filepattern" : ^void (NSString* s, SearchSettings *ss) {
            [ss.inFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"in-filetype" : ^void (NSString* s, SearchSettings *ss) {
            [ss addInFileType:s];
        },
        @"in-linesafterpattern" : ^void (NSString* s, SearchSettings *ss) {
            [ss.inLinesAfterPatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"in-linesbeforepattern" : ^void (NSString* s, SearchSettings *ss) {
            [ss.inLinesBeforePatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"linesaftertopattern" : ^void (NSString* s, SearchSettings *ss) {
            [ss.linesAfterToPatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"linesafteruntilpattern" : ^void (NSString* s, SearchSettings *ss) {
            [ss.linesAfterUntilPatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"maxlastmod" : ^void (NSString* s, SearchSettings *ss) {
            [ss setMaxLastModFromString:s];
        },
        @"minlastmod" : ^void (NSString* s, SearchSettings *ss) {
            [ss setMinLastModFromString:s];
        },
        @"out-archiveext" : ^void (NSString* s, SearchSettings *ss) {
            [ss addOutArchiveExtension:s];
        },
        @"out-archivefilepattern" : ^void (NSString* s, SearchSettings *ss) {
            [ss.outArchiveFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"out-dirpattern" : ^void (NSString* s, SearchSettings *ss) {
            [ss.outDirPatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"out-ext" : ^void (NSString* s, SearchSettings *ss) {
            [ss addOutExtension:s];
        },
        @"out-filepattern" : ^void (NSString* s, SearchSettings *ss) {
            [ss.outFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"out-filetype" : ^void (NSString* s, SearchSettings *ss) {
            [ss addOutFileType:s];
        },
        @"out-linesafterpattern" : ^void (NSString* s, SearchSettings *ss) {
            [ss.outLinesAfterPatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"out-linesbeforepattern" : ^void (NSString* s, SearchSettings *ss) {
            [ss.outLinesBeforePatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"path" : ^void (NSString* s, SearchSettings *ss) {
            [ss addPath:s];
        },
        @"searchpattern" : ^void (NSString* s, SearchSettings *ss) {
            [ss.searchPatterns addObject:[[Regex alloc] initWithPattern:s]];
        },
        @"sort-by" : ^void (NSString* s, SearchSettings *ss) {
            [ss setSortByFromName:s];
        }
    };
}

typedef void (^IntegerActionBlockType)(NSInteger, SearchSettings*);

- (NSDictionary<NSString*,IntegerActionBlockType>*) getIntegerActionDict {
    return @{
        @"linesafter" : [^void (NSInteger i, SearchSettings *ss) { ss.linesAfter = i; } copy],
        @"linesbefore" : [^void (NSInteger i, SearchSettings *ss) { ss.linesBefore = i; } copy],
        @"maxdepth" : [^void (NSInteger i, SearchSettings *ss) { ss.maxDepth = i; } copy],
        @"maxlinelength" : [^void (NSInteger i, SearchSettings *ss) { ss.maxLineLength = i; } copy],
        @"maxsize" : [^void (NSInteger i, SearchSettings *ss) { ss.maxSize = i; } copy],
        @"mindepth" : [^void (NSInteger i, SearchSettings *ss) { ss.minDepth = i; } copy],
        @"minsize" : [^void (NSInteger i, SearchSettings *ss) { ss.minSize = i; } copy]
    };
}

- (void) applyArgTokenToSettings:(ArgToken *)argToken settings:(SearchSettings *)settings error:(NSError **)error {
    if (argToken.type == ArgTokenTypeBool) {
        if ([argToken.value isKindOfClass:[NSNumber class]]) {
            NSNumber *num = (NSNumber *)argToken.value;
            BOOL b = [num boolValue];
            void(^block)(BOOL, FindSettings*) = self.boolActionDict[argToken.name];
            block(b, settings);
        } else {
            setError(error, [@"Invalid value for option: " stringByAppendingString:argToken.name]);
            return;
        }
    } else if (argToken.type == ArgTokenTypeStr) {
        if ([argToken.value isKindOfClass:[NSString class]]) {
            NSString *s = (NSString *)argToken.value;
            void(^block)(NSString *, FindSettings *) = self.stringActionDict[argToken.name];
            block(s, settings);
        } else if ([argToken.value isKindOfClass:[NSArray class]]) {
            NSArray *arr = (NSArray *)argToken.value;
            for (NSObject *o in arr) {
                if ([o isKindOfClass:[NSString class]]) {
                    NSString *s = (NSString *)argToken.value;
                    void(^block)(NSString *, FindSettings *) = self.stringActionDict[argToken.name];
                    block(s, settings);
                } else {
                    setError(error, [@"Invalid value for option: " stringByAppendingString:argToken.name]);
                    return;
                }
            }
        } else {
            setError(error, [@"Invalid value for option: " stringByAppendingString:argToken.name]);
            return;
        }
    } else if (argToken.type == ArgTokenTypeInt) {
        if ([argToken.value isKindOfClass:[NSNumber class]]) {
            NSNumber *num = (NSNumber *)argToken.value;
            NSInteger i = [num integerValue];
            void(^block)(NSInteger, FindSettings*) = self.integerActionDict[argToken.name];
            block(i, settings);
        } else {
            setError(error, [@"Invalid value for option: " stringByAppendingString:argToken.name]);
            return;
        }
    } else {
        setError(error, [@"Invalid option: " stringByAppendingString:argToken.name]);
        return;
    }
}

- (void) updateSettingsFromArgTokens:(SearchSettings *)settings argTokens:(NSArray<ArgToken*> *)argTokens error:(NSError **)error {
    for (ArgToken *argToken in argTokens) {
        if ([argToken.name isEqualToString:@"settings-file"]) {
            [self updateSettingsFromFile:settings filePath:[NSString stringWithFormat:@"%@", argToken.value] error:error];
        } else {
            [self applyArgTokenToSettings:argToken settings:settings error:error];
        }
        if (*error) {
            return;
        }
    }
}

- (void) updateSettingsFromDictionary:(SearchSettings *)settings dictionary:(NSDictionary *)dictionary error:(NSError **)error {
    NSArray<ArgToken*> *argTokens = [self.argTokenizer tokenizeDictionary:dictionary error:error];
    if (*error) {
        return;
    }
    [self updateSettingsFromArgTokens:settings argTokens:argTokens error:error];
}

- (void) updateSettingsFromData:(SearchSettings *)settings data:(NSData *)data error:(NSError **)error {
    NSArray<ArgToken*> *argTokens = [self.argTokenizer tokenizeData:data error:error];
    if (*error) {
        return;
    }
    [self updateSettingsFromArgTokens:settings argTokens:argTokens error:error];
}

- (FindSettings *) settingsFromData:(NSData *)data error:(NSError **)error {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [self updateSettingsFromData:settings data:data error:error];
    return settings;
}

- (void) updateSettingsFromFile:(SearchSettings *)settings filePath:(NSString *)filePath error:(NSError **)error {
    NSArray<ArgToken*> *argTokens = [self.argTokenizer tokenizeFile:filePath error:error];
    if (*error) {
        return;
    }
    [self updateSettingsFromArgTokens:settings argTokens:argTokens error:error];
}

- (SearchSettings *) settingsFromFile:(NSString *)filePath error:(NSError **)error {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [self updateSettingsFromFile:settings filePath:filePath error:error];
    return settings;
}

- (void) updateSettingsFromArgs:(SearchSettings *)settings args:(NSArray *)args error:(NSError **)error {
    NSArray<ArgToken*> *argTokens = [self.argTokenizer tokenizeArgs:args error:error];
    if (*error) {
        return;
    }
    [self updateSettingsFromArgTokens:settings argTokens:argTokens error:error];
}

- (SearchSettings *) settingsFromArgs:(NSArray<NSString*> *)args error:(NSError **)error {
    SearchSettings *settings = [[SearchSettings alloc] init];
    // default printResults to true since running as cli
    settings.printResults = true;
    [self updateSettingsFromArgs:settings args:args error:error];
    return settings;
}

- (NSString*) getUsageString {
    NSMutableString *s = [[NSMutableString alloc] initWithString:@"\nUsage:\n"];
    [s appendString:@" objcsearch [options] -s <searchpattern> <path> [<path> ...]\n\n"];
    [s appendString:@"Options:\n"];
    NSMutableArray *optStrings = [NSMutableArray array];
    long longest = 0;
    NSArray *sortedOptions = [self.searchOptions sortedArrayUsingComparator:^NSComparisonResult(SearchOption *so1, SearchOption *so2) {
        return [[so1 sortArg] compare:[so2 sortArg]];
    }];
    for (SearchOption *so in sortedOptions) {
        NSMutableString *optString = [[NSMutableString alloc] init];
        if (so.shortArg) {
            [optString appendFormat:@"-%@,", so.shortArg];
        }
        [optString appendFormat:@"--%@", so.longArg];
        if ([optString length] > longest) {
            longest = [optString length];
        }
        [optStrings addObject:optString];
    }
    // For some reason, length-specified fields don't work in NSString format strings,
    // so forced to use char * and sprintf
    char *metaString = " %%-%lus  %%s\n";
    char templateString[20];
    sprintf(templateString, metaString, longest);
    for (int i=0; i < [sortedOptions count]; i++) {
        NSString *optString = optStrings[i];
        NSString *optDesc = [sortedOptions[i] desc];
        long formatLen = [optString length] + [optDesc length] + 5;
        char formatString[formatLen];
        sprintf(formatString, templateString, [optString UTF8String], [optDesc UTF8String]);
        [s appendString:[NSString stringWithUTF8String:formatString]];
    }
    return [NSString stringWithString:s];
}

- (void) usage:(int)code {
    logMsg([self getUsageString]);
    exit(code);
}

@end
