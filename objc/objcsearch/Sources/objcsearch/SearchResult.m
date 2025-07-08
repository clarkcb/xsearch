#import "SearchResult.h"
#import "color.h"

@implementation SearchResult

- (instancetype) initWithPattern:(NSString*)pattern
                            file:(FileResult*)file
                         lineNum:(unsigned long)lineNum
                 matchStartIndex:(unsigned long)matchStartIndex
                   matchEndIndex:(unsigned long)matchEndIndex
                            line:(NSString*)line
                     linesBefore:(NSArray<NSString*>*)linesBefore
                      linesAfter:(NSArray<NSString*>*)linesAfter
{
    self = [super init];
    if (self) {
        self.searchPattern = pattern;
        self.file = file;
        self.lineNum = lineNum;
        self.matchStartIndex = matchStartIndex;
        self.matchEndIndex = matchEndIndex;
        self.line = line;
        self.linesBefore = linesBefore;
        self.linesAfter = linesAfter;
    }
    return self;
}

- (NSString *) getFilePath {
    if (self.file != nil) {
        return [self.file description];
    } else {
        return @"<text>";
    }
}

- (NSComparisonResult)compareBySearchFields:(SearchResult *)otherSearchResult {
    if (self.lineNum == otherSearchResult.lineNum) {
        if (self.matchStartIndex == otherSearchResult.matchStartIndex) {
            if (self.matchEndIndex == otherSearchResult.matchEndIndex) {
                return NSOrderedSame;
            }
            if (self.matchEndIndex < otherSearchResult.matchEndIndex) {
                return NSOrderedAscending;
            }
            return NSOrderedDescending;
        }
        if (self.matchStartIndex < otherSearchResult.matchStartIndex) {
            return NSOrderedAscending;
        }
        return NSOrderedDescending;
    }
    if (self.lineNum < otherSearchResult.lineNum) {
        return NSOrderedAscending;
    }
    return NSOrderedDescending;
}

- (NSComparisonResult)compareByPath:(SearchResult *)otherSearchResult caseInsensitive:(BOOL)caseInsensitive {
    if (self.file != nil && otherSearchResult.file != nil) {
        NSComparisonResult res = [self.file compareByPath:otherSearchResult.file caseInsensitive:caseInsensitive];
        if (res != NSOrderedSame) return res;
    }
    return [self compareBySearchFields:otherSearchResult];
}

- (NSComparisonResult)compareByName:(SearchResult *)otherSearchResult caseInsensitive:(BOOL)caseInsensitive {
    if (self.file != nil && otherSearchResult.file != nil) {
        NSComparisonResult res = [self.file compareByName:otherSearchResult.file caseInsensitive:caseInsensitive];
        if (res != NSOrderedSame) return res;
    }
    return [self compareBySearchFields:otherSearchResult];
}

- (NSComparisonResult)compareBySize:(SearchResult *)otherSearchResult caseInsensitive:(BOOL)caseInsensitive {
    if (self.file != nil && otherSearchResult.file != nil) {
        NSComparisonResult res = [self.file compareBySize:otherSearchResult.file caseInsensitive:caseInsensitive];
        if (res != NSOrderedSame) return res;
    }
    return [self compareBySearchFields:otherSearchResult];
}

- (NSComparisonResult)compareByType:(SearchResult *)otherSearchResult caseInsensitive:(BOOL)caseInsensitive {
    if (self.file != nil && otherSearchResult.file != nil) {
        NSComparisonResult res = [self.file compareByType:otherSearchResult.file caseInsensitive:caseInsensitive];
        if (res != NSOrderedSame) return res;
    }
    return [self compareBySearchFields:otherSearchResult];
}

- (NSComparisonResult)compareByLastMod:(SearchResult *)otherSearchResult caseInsensitive:(BOOL)caseInsensitive {
    if (self.file != nil && otherSearchResult.file != nil) {
        NSComparisonResult res = [self.file compareByLastMod:otherSearchResult.file caseInsensitive:caseInsensitive];
        if (res != NSOrderedSame) return res;
    }
    return [self compareBySearchFields:otherSearchResult];
}

@end
