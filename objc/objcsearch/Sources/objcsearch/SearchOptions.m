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

@end

@implementation SearchOptions

- (instancetype) init {
    self = [super init];
    if (self) {
        self.searchOptions = [self searchOptionsFromJson];
        self.longArgDict = [self getLongArgDict];
        self.boolActionDict = [self getBoolActionDict];
        self.stringActionDict = [self getStringActionDict];
        self.integerActionDict = [self getIntegerActionDict];
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
                SearchOption *so = [[SearchOption alloc] initWithShortArg:sArg withLongArg:lArg withDesc:desc];
                [searchOptions addObject:(SearchOption*)so];
            }
        }
    }
    NSArray *sortedOptions = [searchOptions sortedArrayUsingComparator:^NSComparisonResult(SearchOption *so1, SearchOption *so2) {
        return [[so1 sortArg] compare:[so2 sortArg]];
    }];

    return sortedOptions;
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

- (void) applySetting:(NSString *)name obj:(NSObject *)obj settings:(SearchSettings *)settings error:(NSError **)error {
    if (self.longArgDict[name]) {
        if (self.boolActionDict[name]) {
            if ([obj isKindOfClass:[NSNumber class]]) {
                NSNumber *num = (NSNumber *)obj;
                BOOL b = [num boolValue];
                void(^block)(BOOL, SearchSettings*) = self.boolActionDict[name];
                block(b, settings);
            } else {
                setError(error, [@"Invalid value for option: " stringByAppendingString:name]);
                return;
            }
        } else if (self.stringActionDict[name]) {
            if ([obj isKindOfClass:[NSString class]]) {
                NSString *s = (NSString *)obj;
                void(^block)(NSString *, SearchSettings *) = self.stringActionDict[name];
                block(s, settings);
            } else if ([obj isKindOfClass:[NSArray class]]) {
                NSArray *arr = (NSArray *)obj;
                for (NSObject *o in arr) {
                    [self applySetting:name obj:o settings:settings error:error];
                    if (*error) {
                        return;
                    }
                }
            } else {
                setError(error, [@"Invalid value for option: " stringByAppendingString:name]);
                return;
            }
        } else if (self.integerActionDict[name]) {
            if ([obj isKindOfClass:[NSNumber class]]) {
                NSNumber *num = (NSNumber *)obj;
                NSInteger i = [num integerValue];
                void(^block)(NSInteger, SearchSettings*) = self.integerActionDict[name];
                block(i, settings);
            } else {
                setError(error, [@"Invalid value for option: " stringByAppendingString:name]);
                return;
            }
        } else {
            setError(error, [@"Invalid option: " stringByAppendingString:name]);
            return;
        }
    } else {
        setError(error, [@"Invalid option: " stringByAppendingString:name]);
    }
}

- (void) updateSettingsFromData:(NSData *)data settings:(SearchSettings *)settings error:(NSError **)error {
    if (NSClassFromString(@"NSJSONSerialization")) {
        id jsonObject = [NSJSONSerialization
                         JSONObjectWithData:data
                         options:0
                         error:error];
        
        if (*error) {
            setError(error, @"Unable to parse JSON");
            return;
        }
        
        if (![jsonObject isKindOfClass:[NSDictionary class]]) {
            setError(error, @"Invalid JSON");
            return;
        }

        // keys are sorted so that output is consistent across all versions
        NSArray<NSString*> *keys = [[jsonObject allKeys] sortedArrayUsingSelector:@selector(compare:)];
        // First check for invalid keys
        for (NSString *key in keys) {
            if (!self.longArgDict[key]) {
                setError(error, [@"Invalid option: " stringByAppendingString:key]);
                return;
            }
        }
        for (NSString *key in keys) {
            NSObject *val = jsonObject[key];
            [self applySetting:key obj:val settings:settings error:error];
            if (*error) {
                return;
            }
        }
    }
}

- (SearchSettings *) settingsFromData:(NSData *)data error:(NSError **)error {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [self updateSettingsFromData:data settings:settings error:error];
    return settings;
}

- (void) updateSettingsFromFile:(NSString *)settingsFilePath settings:(SearchSettings *)settings error:(NSError **)error {
    NSString *expandedPath = [FileUtil expandPath:settingsFilePath];
    if (![[NSFileManager defaultManager] fileExistsAtPath:expandedPath]) {
        setError(error, [@"Settings file not found: " stringByAppendingString:settingsFilePath]);
        return;
    }
    if (![expandedPath hasSuffix:@".json"]) {
        setError(error, [@"Invalid settings file (must be JSON): " stringByAppendingString:settingsFilePath]);
        return;
    }
    NSData *data = [NSData dataWithContentsOfFile:expandedPath];
    [self updateSettingsFromData:data settings:settings error:error];
    if (*error) {
        if ([[*error domain] isEqualToString:@"Unable to parse JSON"] || [[*error domain] isEqualToString:@"Invalid JSON"]) {
            NSString *newErr = [[[*error domain] stringByAppendingString:@" in settings file: "] stringByAppendingString:settingsFilePath];
            setError(error, newErr);
        }
    }
}

- (SearchSettings *) settingsFromFile:(NSString *)settingsFilePath error:(NSError **)error {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [self updateSettingsFromFile:settingsFilePath settings:settings error:error];
    return settings;
}

- (SearchSettings *) settingsFromArgs:(NSArray<NSString*> *)args error:(NSError **)error {
    SearchSettings *settings = [[SearchSettings alloc] init];
    // default printResults to true since running as cli
    settings.printResults = true;

    int i = 1;
    while (i < [args count]) {
        if (*error) {
            return nil;
        }
        NSString *arg = args[i];
        if ([arg hasPrefix:@"-"]) {
            while ([arg hasPrefix:@"-"] && [arg length] > 1) {
                arg = [arg substringFromIndex:1];
            }
            if (self.longArgDict[arg]) {
                NSString *longArg = self.longArgDict[arg];
                if (self.boolActionDict[longArg]) {
                    void(^block)(BOOL, SearchSettings *) = self.boolActionDict[longArg];
                    block(true, settings);
                } else {
                    NSString *argVal = @"";
                    if ([args count] > i+1) {
                        argVal = args[i+1];
                        i++;
                    } else {
                        setError(error, [NSString stringWithFormat:@"Missing argument for option %@", arg]);
                        return nil;
                    }
                    if (self.stringActionDict[longArg]) {
                        void(^block)(NSString *, SearchSettings *) = self.stringActionDict[longArg];
                        block(argVal, settings);
                    } else if (self.integerActionDict[longArg]) {
                        void(^block)(NSInteger, SearchSettings *) = self.integerActionDict[longArg];
                        block([argVal integerValue], settings);
                    } else if ([longArg isEqualToString:@"settings-file"]) {
                        [self updateSettingsFromFile:argVal settings:settings error:error];
                        if (*error) {
                            return nil;
                        }
                    } else {
                        setError(error, [NSString stringWithFormat:@"Invalid option: %@", arg]);
                        return nil;
                    }
                }
            } else {
                setError(error, [NSString stringWithFormat:@"Invalid option: %@", arg]);
                return nil;
            }
        } else {
            [settings addPath:args[i]];
        }
        i++;
    }
    
    return settings;
}

- (NSString*) getUsageString {
    NSMutableString *s = [[NSMutableString alloc] initWithString:@"\nUsage:\n"];
    [s appendString:@" objcsearch [options] -s <searchpattern> <path> [<path> ...]\n\n"];
    [s appendString:@"Options:\n"];
    NSMutableArray *optStrings = [NSMutableArray array];
    long longest = 0;
    for (SearchOption *so in self.searchOptions) {
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
    for (int i=0; i < [self.searchOptions count]; i++) {
        NSString *optString = optStrings[i];
        NSString *optDesc = self.searchOptions[i].desc;
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
