#import "SearchSettings.h"

@implementation SearchSettings

- (instancetype) init {
    self = [super init];
    if (self) {
        self.colorize = true;
        self.firstMatch = false;
        self.linesAfter = 0;
        self.linesBefore = 0;
        self.maxLineLength = 150;
        self.multiLineSearch = false;
        self.printLines = false;
        self.printResults = true;
        self.searchArchives = false;
        self.textFileEncoding = @"UTF-8";
        self.uniqueLines = false;

        self.inLinesAfterPatterns = [[NSMutableArray alloc] init];
        self.inLinesBeforePatterns = [[NSMutableArray alloc] init];
        self.linesAfterToPatterns = [[NSMutableArray alloc] init];
        self.linesAfterUntilPatterns = [[NSMutableArray alloc] init];
        self.outLinesAfterPatterns = [[NSMutableArray alloc] init];
        self.outLinesBeforePatterns = [[NSMutableArray alloc] init];
        self.searchPatterns = [[NSMutableArray alloc] init];
    }
    return self;
}

- (NSString *) description {
    NSMutableString *d = [[NSMutableString alloc] initWithString:@"SearchSettings("];
    [d appendFormat:@"archivesOnly=%@", boolToNSString(self.archivesOnly)];
    [d appendFormat:@", colorize=%@", boolToNSString(self.colorize)];
    [d appendFormat:@", debug=%@", boolToNSString(self.debug)];
    [d appendFormat:@", firstMatch=%@", boolToNSString(self.firstMatch)];
    [d appendFormat:@", followSymlinks=%@", boolToNSString(self.followSymlinks)];
    [d appendFormat:@", inArchiveExtensions=%@", arrayToNSString(self.inArchiveExtensions)];
    [d appendFormat:@", inArchiveFilePatterns=%@", arrayToNSString(self.inArchiveFilePatterns)];
    [d appendFormat:@", includeHidden=%@", boolToNSString(self.includeHidden)];
    [d appendFormat:@", inDirPatterns=%@", arrayToNSString(self.inDirPatterns)];
    [d appendFormat:@", inExtensions=%@", arrayToNSString(self.inExtensions)];
    [d appendFormat:@", inFilePatterns=%@", arrayToNSString(self.inFilePatterns)];
    [d appendFormat:@", inFileTypes=%@", [FindSettings fileTypesArrayToNSString:self.inFileTypes]];
    [d appendFormat:@", inLinesAfterPatterns=%@", arrayToNSString(self.inLinesAfterPatterns)];
    [d appendFormat:@", inLinesBeforePatterns=%@", arrayToNSString(self.inLinesBeforePatterns)];
    [d appendFormat:@", linesAfterToPatterns=%@", arrayToNSString(self.linesAfterToPatterns)];
    [d appendFormat:@", linesAfterUntilPatterns=%@", arrayToNSString(self.linesAfterUntilPatterns)];
    [d appendFormat:@", linesAfter=%d", self.linesAfter];
    [d appendFormat:@", linesBefore=%d", self.linesBefore];
    [d appendFormat:@", maxDepth=%ld", (long)self.maxDepth];
    [d appendFormat:@", maxLastMod=%@", [FindSettings lastModToNSString:self.maxLastMod]];
    [d appendFormat:@", maxLineLength=%d", self.maxLineLength];
    [d appendFormat:@", maxSize=%lu", (long)self.maxSize];
    [d appendFormat:@", minDepth=%ld", (long)self.minDepth];
    [d appendFormat:@", minLastMod=%@", [FindSettings lastModToNSString:self.minLastMod]];
    [d appendFormat:@", minSize=%lu", (long)self.minSize];
    [d appendFormat:@", outArchiveExtensions=%@", arrayToNSString(self.outArchiveExtensions)];
    [d appendFormat:@", outArchiveFilePatterns=%@", arrayToNSString(self.outArchiveFilePatterns)];
    [d appendFormat:@", outDirPatterns=%@", arrayToNSString(self.outDirPatterns)];
    [d appendFormat:@", outExtensions=%@", arrayToNSString(self.outExtensions)];
    [d appendFormat:@", outFilePatterns=%@", arrayToNSString(self.outFilePatterns)];
    [d appendFormat:@", outFileTypes=%@", [FindSettings fileTypesArrayToNSString:self.outFileTypes]];
    [d appendFormat:@", outLinesAfterPatterns=%@", arrayToNSString(self.outLinesAfterPatterns)];
    [d appendFormat:@", outLinesBeforePatterns=%@", arrayToNSString(self.outLinesBeforePatterns)];
    [d appendFormat:@", paths=%@", arrayToNSString(self.paths)];
    [d appendFormat:@", printDirs=%@", boolToNSString(self.printDirs)];
    [d appendFormat:@", printFiles=%@", boolToNSString(self.printFiles)];
    [d appendFormat:@", printLines=%@", boolToNSString(self.printLines)];
    [d appendFormat:@", printResults=%@", boolToNSString(self.printResults)];
    [d appendFormat:@", printUsage=%@", boolToNSString(self.printUsage)];
    [d appendFormat:@", printVersion=%@", boolToNSString(self.printVersion)];
    [d appendFormat:@", recursive=%@", boolToNSString(self.recursive)];
    [d appendFormat:@", searchArchives=%@", boolToNSString(self.searchArchives)];
    [d appendFormat:@", searchPatterns=%@", arrayToNSString(self.searchPatterns)];
    [d appendFormat:@", sortBy=%@", [FindSettings getNameFromSortBy:self.sortBy]];
    [d appendFormat:@", sortCaseInsensitive=%@", boolToNSString(self.sortCaseInsensitive)];
    [d appendFormat:@", sortDescending=%@", boolToNSString(self.sortDescending)];
    [d appendFormat:@", textFileEncoding=\"%@\"", self.textFileEncoding];
    [d appendFormat:@", uniqueLines=%@", boolToNSString(self.uniqueLines)];
    [d appendFormat:@", verbose=%@", boolToNSString(self.verbose)];
    [d appendString:@")"];
    return d;
}

- (void) addPattern:(NSString *)pattern toArr:(NSMutableArray *)arr {
    [arr addObject:[[Regex alloc] initWithPattern:pattern]];
}

- (void) addSearchPattern:(NSString *)pattern {
    [self addPattern:pattern toArr:self.searchPatterns];
}

//- (BOOL) archivesOnly {
//    return _archivesOnly;
//}

- (void)setArchivesOnly:(BOOL)b {
    [super setArchivesOnly:b];
//    _archivesOnly = b;
    if (b) {
        [self setSearchArchives:b];
    }
}

@end
