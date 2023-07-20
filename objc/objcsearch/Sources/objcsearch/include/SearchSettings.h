#ifndef SearchSettings_h
#define SearchSettings_h

#import <Foundation/Foundation.h>
#import "common.h"
#import "FileTypes.h"
#import "FindSettings.h"
#import "Regex.h"

@interface SearchSettings : FindSettings

@property(nonatomic) BOOL colorize;
@property(nonatomic) BOOL firstMatch;
@property(nonatomic) int linesAfter;
@property(nonatomic) int linesBefore;
@property(nonatomic) BOOL listLines;
@property(nonatomic) uint32_t maxLineLength;
@property(nonatomic) BOOL multiLineSearch;
@property(nonatomic) BOOL printResults;
@property(nonatomic) BOOL searchArchives;
@property(nonatomic) NSString *textFileEncoding;
@property(nonatomic) BOOL uniqueLines;

@property(nonatomic) NSMutableArray<Regex*> *inLinesAfterPatterns;
@property(nonatomic) NSMutableArray<Regex*> *inLinesBeforePatterns;
@property(nonatomic) NSMutableArray<Regex*> *linesAfterToPatterns;
@property(nonatomic) NSMutableArray<Regex*> *linesAfterUntilPatterns;
@property(nonatomic) NSMutableArray<Regex*> *outLinesAfterPatterns;
@property(nonatomic) NSMutableArray<Regex*> *outLinesBeforePatterns;
@property(nonatomic) NSMutableArray<Regex*> *searchPatterns;

-(NSString *) description;

- (void) addSearchPattern: (NSString*)pattern;

//- (void) setArchivesOnly: (BOOL)b;

@end

#endif /* SearchSettings_h */
