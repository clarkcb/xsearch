#import "config.h"
#import "Regex.h"
#import "SearchOptions.h"

@interface SearchOptions ()
// private properties
@property NSArray<SearchOption*> *searchOptions;
@property NSDictionary<NSString*,NSString*> *longArgDict;
@property NSDictionary *argActionDict;
@property NSDictionary *boolFlagActionDict;

@end

@implementation SearchOptions

- (instancetype) init {
    self = [super init];
    if (self) {
        self.searchOptions = [self searchOptionsFromJson];
        self.longArgDict = [self getLongArgDict];
        self.argActionDict = [self getArgActionDict];
        self.boolFlagActionDict = [self getBoolFlagActionDict];
    }
    return self;
}

- (NSArray<SearchOption*>*) searchOptionsFromJson {
    NSMutableString *searchOptionsJsonPath = [NSMutableString stringWithUTF8String:SHAREDPATH];
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

- (void) applySetting:(NSString *)name obj:(NSObject *)obj settings:(SearchSettings *)settings {
    if ([obj isKindOfClass:[NSString class]]) {
        if ([name isEqualToString:@"path"]) {
            [settings addPath:(NSMutableString*)obj];
        } else if (self.argActionDict[name]) {
            void(^block)() = self.argActionDict[name];
            block(obj, settings);
        }
    } else if ([obj isKindOfClass:[NSNumber class]]) {
        NSNumber *num = (NSNumber *)obj;
        if (self.argActionDict[name]) {
            void(^block)(NSString* s, SearchSettings* ss) = self.argActionDict[name];
            block([num description], settings);
        } else if (self.boolFlagActionDict[name]) {
            BOOL b = [num boolValue];
            void(^block)() = self.boolFlagActionDict[name];
            block(b, settings);
        }
    } else if ([obj isKindOfClass:[NSArray class]]) {
        if (self.argActionDict[name]) {
            void(^block)() = self.argActionDict[name];
            
            NSArray *arr = (NSArray *)obj;
            for (NSObject *o in arr) {
                block([o description], settings);
            }
        }
    }
}

- (void) settingsFromFile:(NSString *)settingsFilePath settings:(SearchSettings *)settings {
    if (![[NSFileManager defaultManager] fileExistsAtPath:settingsFilePath]) {
        return;
    }
    
    NSData *data = [NSData dataWithContentsOfFile:settingsFilePath];
    
    [self settingsFromData:data settings:settings];
}

- (void) settingsFromData:(NSData *)data settings:(SearchSettings *)settings {
    if (NSClassFromString(@"NSJSONSerialization")) {
        NSError *error = nil;
        id jsonObject = [NSJSONSerialization
                         JSONObjectWithData:data
                         options:0
                         error:&error];
        
        if (error) { /* JSON was malformed, act appropriately here */ }
        
        if ([jsonObject isKindOfClass:[NSDictionary class]]) {
            for (NSString *key in jsonObject) {
                NSObject *val = jsonObject[key];
                [self applySetting:key obj:val settings:settings];
            }
        }
    }
}

- (NSDictionary<NSString*,NSString*>*) getLongArgDict {
    NSMutableDictionary *longArgDict = [[NSMutableDictionary alloc] initWithCapacity:68];
    for (SearchOption *so in self.searchOptions) {
        longArgDict[so.longArg] = so.longArg;
        if (so.shortArg) {
            longArgDict[so.shortArg] = so.longArg;
        }
    }
    return [NSDictionary dictionaryWithDictionary:longArgDict];
}

typedef void (^ArgActionBlockType)(NSString*, SearchSettings*);

- (NSDictionary<NSString*,ArgActionBlockType>*) getArgActionDict {
    return [[NSDictionary alloc] initWithObjectsAndKeys:
            ^void (NSString* s, SearchSettings *ss) {
                ss.textFileEncoding = s;
            }, @"encoding",
            ^void (NSString* s, SearchSettings *ss) {
                [ss addInArchiveExtension:s];
            }, @"in-archiveext",
            ^void (NSString* s, SearchSettings *ss) {
                [ss.inArchiveFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"in-archivefilepattern",
            ^void (NSString* s, SearchSettings *ss) {
                [ss.inDirPatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"in-dirpattern",
            ^void (NSString* s, SearchSettings *ss) {
                [ss addInExtension:s];
            }, @"in-ext",
            ^void (NSString* s, SearchSettings *ss) {
                [ss.inFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"in-filepattern",
            ^void (NSString* s, SearchSettings *ss) {
                [ss addInFileType:s];
            }, @"in-filetype",
            ^void (NSString* s, SearchSettings *ss) {
                [ss.inLinesAfterPatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"in-linesafterpattern",
            ^void (NSString* s, SearchSettings *ss) {
                [ss.inLinesBeforePatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"in-linesbeforepattern",
            ^void (NSString* s, SearchSettings *ss) {
                ss.linesAfter = [s intValue];
            }, @"linesafter",
            ^void (NSString* s, SearchSettings *ss) {
                [ss.linesAfterToPatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"linesaftertopattern",
            ^void (NSString* s, SearchSettings *ss) {
                [ss.linesAfterUntilPatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"linesafteruntilpattern",
            ^void (NSString* s, SearchSettings *ss) {
                ss.linesBefore = [s intValue];
            }, @"linesbefore",
            ^void (NSString* s, SearchSettings *ss) {
                ss.maxLineLength = [s intValue];
            }, @"maxlinelength",
            ^void (NSString* s, SearchSettings *ss) {
                [ss addOutArchiveExtension:s];
            }, @"out-archiveext",
            ^void (NSString* s, SearchSettings *ss) {
                [ss.outArchiveFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"out-archivefilepattern",
            ^void (NSString* s, SearchSettings *ss) {
                [ss.outDirPatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"out-dirpattern",
            ^void (NSString* s, SearchSettings *ss) {
                [ss addOutExtension:s];
            }, @"out-ext",
            ^void (NSString* s, SearchSettings *ss) {
                [ss.outFilePatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"out-filepattern",
            ^void (NSString* s, SearchSettings *ss) {
                [ss addOutFileType:s];
            }, @"out-filetype",
            ^void (NSString* s, SearchSettings *ss) {
                [ss.outLinesAfterPatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"out-linesafterpattern",
            ^void (NSString* s, SearchSettings *ss) {
                [ss.outLinesBeforePatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"out-linesbeforepattern",
            ^void (NSString* s, SearchSettings *ss) {
                [ss addPath:s];
            }, @"path",
            ^void (NSString* s, SearchSettings *ss) {
                [ss.searchPatterns addObject:[[Regex alloc] initWithPattern:s]];
            }, @"searchpattern",
//            ^void (NSString* s, SearchSettings *ss) {
//                [self settingsFromFile:s settings:ss];
//            }, @"settings-file",
            nil];
}

typedef void (^BoolFlagActionBlockType)(BOOL, SearchSettings*);

- (NSDictionary<NSString*,BoolFlagActionBlockType>*) getBoolFlagActionDict {
    return [[NSDictionary alloc] initWithObjectsAndKeys:
            [^void (BOOL b, SearchSettings *ss) { ss.firstMatch = !b; } copy], @"allmatches",
            [^void (BOOL b, SearchSettings *ss) {
                ss.archivesOnly = b;
                if (b) ss.searchArchives = true;
            } copy], @"archivesonly",
            [^void (BOOL b, SearchSettings *ss) {
                ss.debug = b;
                if (b) ss.verbose = true;
            } copy], @"debug",
            [^void (BOOL b, SearchSettings *ss) { ss.excludeHidden = b; } copy], @"excludehidden",
            [^void (BOOL b, SearchSettings *ss) { ss.firstMatch = b; } copy], @"firstmatch",
            [^void (BOOL b, SearchSettings *ss) { ss.printUsage = b; } copy], @"help",
            [^void (BOOL b, SearchSettings *ss) { ss.excludeHidden = !b; } copy], @"includehidden",
            [^void (BOOL b, SearchSettings *ss) { ss.listDirs = b; } copy], @"listdirs",
            [^void (BOOL b, SearchSettings *ss) { ss.listFiles = b; } copy], @"listfiles",
            [^void (BOOL b, SearchSettings *ss) { ss.listLines = b; } copy], @"listlines",
            [^void (BOOL b, SearchSettings *ss) { ss.multiLineSearch = b; } copy], @"multilinesearch",
            [^void (BOOL b, SearchSettings *ss) { ss.printResults = !b; } copy], @"noprintmatches",
            [^void (BOOL b, SearchSettings *ss) { ss.recursive = !b; } copy], @"norecursive",
            [^void (BOOL b, SearchSettings *ss) { ss.searchArchives = !b; } copy], @"nosearcharchives",
            [^void (BOOL b, SearchSettings *ss) { ss.printResults = b; } copy], @"printmatches",
            [^void (BOOL b, SearchSettings *ss) { ss.recursive = b; } copy], @"recursive",
            [^void (BOOL b, SearchSettings *ss) { ss.searchArchives = b; } copy], @"searcharchives",
            [^void (BOOL b, SearchSettings *ss) { ss.uniqueLines = b; } copy], @"uniquelines",
            [^void (BOOL b, SearchSettings *ss) { ss.verbose = b; } copy], @"verbose",
            [^void (BOOL b, SearchSettings *ss) { ss.printVersion = b; } copy], @"version",
            nil];
}

- (SearchSettings *) settingsFromArgs:(NSArray<NSString*> *)args error:(NSError **)error {
    SearchSettings *settings = [[SearchSettings alloc] init];
    int i = 1;
    while (i < [args count]) {
        NSString *arg = args[i];
        if ([arg hasPrefix:@"-"]) {
            while ([arg hasPrefix:@"-"] && [arg length] > 1) {
                arg = [arg substringFromIndex:1];
            }
            if (self.longArgDict[arg]) {
                //logMsg([NSString stringWithFormat:@"Option in longArgDict: %@", arg]);
                NSString *longArg = self.longArgDict[arg];
                if (self.argActionDict[longArg] || [longArg isEqualToString:@"settings-file"]) {
                    if ([args count] > i+1) {
                        NSString *secondArg = args[i+1];
                        if (self.argActionDict[longArg]) {
                            void(^block)() = self.argActionDict[longArg];
                            block(secondArg, settings);
                        } else {
                            [self settingsFromFile:secondArg settings:settings];
                        }
                        i++;
                    } else {
                        setError(error, [NSString stringWithFormat:@"Missing argument for option %@", arg]);
                        return nil;
                    }
                } else if (self.boolFlagActionDict[longArg]) {
                    void(^block)() = self.boolFlagActionDict[longArg];
                    block(true, settings);
                } else {
                    setError(error, [NSString stringWithFormat:@"Invalid option: %@", arg]);
                    return nil;
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
