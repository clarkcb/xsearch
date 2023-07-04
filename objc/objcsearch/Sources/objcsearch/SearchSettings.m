#import "common.h"
#import "FileTypes.h"
#import "SearchSettings.h"

@implementation SearchSettings

@synthesize archivesOnly = _archivesOnly;
@synthesize debug = _debug;

- (instancetype) init {
    self = [super init];
    if (self) {
        self.archivesOnly = false;
        self.colorize = true;
        self.debug = false;
        self.excludeHidden = true;
        self.firstMatch = false;
        self.linesAfter = 0;
        self.linesBefore = 0;
        self.listDirs = false;
        self.listFiles = false;
        self.listLines = false;
        self.maxLineLength = 150;
        self.multiLineSearch = false;
        self.printResults = true;
        self.printUsage = false;
        self.printVersion = false;
        self.recursive = true;
        self.searchArchives = false;
        self.textFileEncoding = @"UTF-8";
        self.uniqueLines = false;
        self.verbose = false;

        self.inArchiveExtensions = [[NSMutableArray alloc] init];
        self.inArchiveFilePatterns = [[NSMutableArray alloc] init];
        self.inDirPatterns = [[NSMutableArray alloc] init];
        self.inExtensions = [[NSMutableArray alloc] init];
        self.inFilePatterns = [[NSMutableArray alloc] init];
        self.inFileTypes = [[NSMutableArray alloc] init];
        self.inLinesAfterPatterns = [[NSMutableArray alloc] init];
        self.inLinesBeforePatterns = [[NSMutableArray alloc] init];
        self.linesAfterToPatterns = [[NSMutableArray alloc] init];
        self.linesAfterUntilPatterns = [[NSMutableArray alloc] init];
        self.outArchiveExtensions = [[NSMutableArray alloc] init];
        self.outArchiveFilePatterns = [[NSMutableArray alloc] init];
        self.outDirPatterns = [[NSMutableArray alloc] init];
        self.outExtensions = [[NSMutableArray alloc] init];
        self.outFilePatterns = [[NSMutableArray alloc] init];
        self.outFileTypes = [[NSMutableArray alloc] init];
        self.outLinesAfterPatterns = [[NSMutableArray alloc] init];
        self.outLinesBeforePatterns = [[NSMutableArray alloc] init];
        self.paths = [[NSMutableArray alloc] init];
        self.searchPatterns = [[NSMutableArray alloc] init];
    }
    return self;
}

- (NSString *) description {
    NSMutableString *d = [[NSMutableString alloc] initWithString:@"SearchSettings("];
    [d appendFormat:@"archivesOnly=%@", boolToNSString(self.archivesOnly)];
    [d appendFormat:@", colorize=%@", boolToNSString(self.colorize)];
    [d appendFormat:@", debug=%@", boolToNSString(self.debug)];
    [d appendFormat:@", excludeHidden=%@", boolToNSString(self.excludeHidden)];
    [d appendFormat:@", firstMatch=%@", boolToNSString(self.firstMatch)];
    [d appendFormat:@", inArchiveExtensions=%@", arrayToNSString(self.inArchiveExtensions)];
    [d appendFormat:@", inArchiveFilePatterns=%@", arrayToNSString(self.inArchiveFilePatterns)];
    [d appendFormat:@", inDirPatterns=%@", arrayToNSString(self.inDirPatterns)];
    [d appendFormat:@", inExtensions=%@", arrayToNSString(self.inExtensions)];
    [d appendFormat:@", inFilePatterns=%@", arrayToNSString(self.inFilePatterns)];
    [d appendFormat:@", inFileTypes=%@", fileTypesArrayToNSString(self.inFileTypes)];
    [d appendFormat:@", inLinesAfterPatterns=%@", arrayToNSString(self.inLinesAfterPatterns)];
    [d appendFormat:@", inLinesBeforePatterns=%@", arrayToNSString(self.inLinesBeforePatterns)];
    [d appendFormat:@", linesAfterToPatterns=%@", arrayToNSString(self.linesAfterToPatterns)];
    [d appendFormat:@", linesAfterUntilPatterns=%@", arrayToNSString(self.linesAfterUntilPatterns)];
    [d appendFormat:@", linesAfter=%d", self.linesAfter];
    [d appendFormat:@", linesBefore=%d", self.linesBefore];
    [d appendFormat:@", listDirs=%@", boolToNSString(self.listDirs)];
    [d appendFormat:@", listFiles=%@", boolToNSString(self.listFiles)];
    [d appendFormat:@", listLines=%@", boolToNSString(self.listLines)];
    [d appendFormat:@", maxLineLength=%d", self.maxLineLength];
    [d appendFormat:@", outArchiveExtensions=%@", arrayToNSString(self.outArchiveExtensions)];
    [d appendFormat:@", outArchiveFilePatterns=%@", arrayToNSString(self.outArchiveFilePatterns)];
    [d appendFormat:@", outDirPatterns=%@", arrayToNSString(self.outDirPatterns)];
    [d appendFormat:@", outExtensions=%@", arrayToNSString(self.outExtensions)];
    [d appendFormat:@", outFilePatterns=%@", arrayToNSString(self.outFilePatterns)];
    [d appendFormat:@", outFileTypes=%@", fileTypesArrayToNSString(self.outFileTypes)];
    [d appendFormat:@", outLinesAfterPatterns=%@", arrayToNSString(self.outLinesAfterPatterns)];
    [d appendFormat:@", outLinesBeforePatterns=%@", arrayToNSString(self.outLinesBeforePatterns)];
    [d appendFormat:@", paths=%@", arrayToNSString(self.paths)];
    [d appendFormat:@", printResults=%@", boolToNSString(self.printResults)];
    [d appendFormat:@", printUsage=%@", boolToNSString(self.printUsage)];
    [d appendFormat:@", printVersion=%@", boolToNSString(self.printVersion)];
    [d appendFormat:@", recursive=%@", boolToNSString(self.recursive)];
    [d appendFormat:@", searchArchives=%@", boolToNSString(self.searchArchives)];
    [d appendFormat:@", searchPatterns=%@", arrayToNSString(self.searchPatterns)];
    [d appendFormat:@", textFileEncoding=\"%@\"", self.textFileEncoding];
    [d appendFormat:@", uniqueLines=%@", boolToNSString(self.uniqueLines)];
    [d appendFormat:@", verbose=%@", boolToNSString(self.verbose)];
    [d appendString:@")"];
    return d;
}

- (void) addExtensions:(NSString *)ext toArr:(NSMutableArray *)arr {
    NSArray *exts = [ext componentsSeparatedByString:@","];
    [arr addObjectsFromArray:exts];
}

- (void) addPattern:(NSString *)pattern toArr:(NSMutableArray *)arr {
    [arr addObject:[[Regex alloc] initWithPattern:pattern]];
}

- (void) addInArchiveExtension:(NSString *)ext {
    [self addExtensions:ext toArr:self.inArchiveExtensions];
}

- (void) addInArchiveFilePattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.inArchiveFilePatterns];
}

- (void) addInDirPattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.inDirPatterns];
}

- (void) addInExtension:(NSString *)ext {
    [self addExtensions:ext toArr:self.inExtensions];
}

- (void) addInFilePattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.inFilePatterns];
}

- (void) addOutArchiveExtension:(NSString *)ext {
    [self addExtensions:ext toArr:self.outArchiveExtensions];
}

- (void) addOutArchiveFilePattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.outArchiveFilePatterns];
}

- (void) addOutDirPattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.outDirPatterns];
}

- (void) addOutExtension:(NSString *)ext {
    [self addExtensions:ext toArr:self.outExtensions];
}

- (void) addOutFilePattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.outFilePatterns];
}

- (void) addFileType:(NSString *)typeName toArr:(NSMutableArray *)arr {
    FileType fileType = [FileTypes fromName:typeName];
    [arr addObject:[NSNumber numberWithInt:fileType]];
    // if fileType is FileTypeText, add text sub-types
    if (fileType == FileTypeText) {
        [arr addObject:[NSNumber numberWithInt:FileTypeCode]];
        [arr addObject:[NSNumber numberWithInt:FileTypeXml]];
    }
}

- (void) addInFileType:(NSString *)typeName {
    [self addFileType:typeName toArr:self.inFileTypes];
}

- (void) addOutFileType:(NSString *)typeName {
    [self addFileType:typeName toArr:self.outFileTypes];
}

- (void) addPath:(NSString *)path {
    [self.paths addObject:path];
}

- (void) addSearchPattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.searchPatterns];
}

- (BOOL) archivesOnly {
    return _archivesOnly;
}

- (void)setArchivesOnly:(BOOL)b {
    _archivesOnly = b;
    if (b) {
        [self setSearchArchives:b];
    }
}

- (BOOL) debug {
    return _debug;
}

- (void)setDebug:(BOOL)b {
    _debug = b;
    if (b) {
        [self setVerbose:b];
    }
}

NSString* fileTypesArrayToNSString(NSArray<NSNumber*> *arr) {
    NSMutableString *arrString = [NSMutableString stringWithString:@"["];
    for (int i=0; i < [arr count]; i++) {
        if (i > 0) {
            [arrString appendString:@", "];
        }
        NSString *typeName = [FileTypes toName:[arr[i] intValue]];
        [arrString appendFormat:@"%@", typeName];
    }
    [arrString appendString:@"]"];
    return [NSString stringWithString:arrString];
}

@end
