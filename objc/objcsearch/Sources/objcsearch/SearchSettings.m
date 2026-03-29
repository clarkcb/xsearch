#import "SearchSettings.h"

@implementation SearchSettings

- (instancetype) init {
    self = [super init];
    if (self) {
        self.firstMatch = false;
        self.lineColor = ColorGreen;
        self.linesAfter = 0;
        self.linesBefore = 0;
        self.maxLineLength = 150;
        self.multiLineSearch = false;
        self.printLines = false;
        self.printMatches = false;
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
