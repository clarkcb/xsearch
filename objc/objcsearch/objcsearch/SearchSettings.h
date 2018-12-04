#import <Foundation/Foundation.h>
#import "common.h"
#import "FileTypes.h"
#import "Regex.h"

@interface SearchSettings : NSObject
//@private
//    BOOL _archivesOnly;
//    BOOL _debug;
//    NSMutableString *_startPath;
//}

@property(nonatomic) BOOL archivesOnly;
@property(nonatomic) BOOL debug;
@property(nonatomic) BOOL excludeHidden;
@property(nonatomic) BOOL firstMatch;
@property(nonatomic) int linesAfter;
@property(nonatomic) int linesBefore;
@property(nonatomic) BOOL listDirs;
@property(nonatomic) BOOL listFiles;
@property(nonatomic) BOOL listLines;
@property(nonatomic) int maxLineLength;
@property(nonatomic) BOOL multiLineSearch;
@property(nonatomic) BOOL printResults;
@property(nonatomic) BOOL printUsage;
@property(nonatomic) BOOL printVersion;
@property(nonatomic) BOOL recursive;
@property(nonatomic) BOOL searchArchives;
@property(nonatomic) NSMutableString *startPath;
@property(nonatomic) BOOL uniqueLines;
@property(nonatomic) BOOL verbose;

@property(nonatomic) NSMutableArray<NSString*> *inArchiveExtensions;
@property(nonatomic) NSMutableArray<Regex*> *inArchiveFilePatterns;
@property(nonatomic) NSMutableArray<Regex*> *inDirPatterns;
@property(nonatomic) NSMutableArray<NSString*> *inExtensions;
@property(nonatomic) NSMutableArray<Regex*> *inFilePatterns;
@property(nonatomic) NSMutableArray<NSNumber*> *inFileTypes;
@property(nonatomic) NSMutableArray<Regex*> *inLinesAfterPatterns;
@property(nonatomic) NSMutableArray<Regex*> *inLinesBeforePatterns;
@property(nonatomic) NSMutableArray<Regex*> *linesAfterToPatterns;
@property(nonatomic) NSMutableArray<Regex*> *linesAfterUntilPatterns;
@property(nonatomic) NSMutableArray<NSString*> *outArchiveExtensions;
@property(nonatomic) NSMutableArray<Regex*> *outArchiveFilePatterns;
@property(nonatomic) NSMutableArray<Regex*> *outDirPatterns;
@property(nonatomic) NSMutableArray<NSString*> *outExtensions;
@property(nonatomic) NSMutableArray<Regex*> *outFilePatterns;
@property(nonatomic) NSMutableArray<NSNumber*> *outFileTypes;
@property(nonatomic) NSMutableArray<Regex*> *outLinesAfterPatterns;
@property(nonatomic) NSMutableArray<Regex*> *outLinesBeforePatterns;
@property(nonatomic) NSMutableArray<Regex*> *searchPatterns;

-(NSString *) description;

- (void) addExtensions:(NSString*)ext toArr:(NSMutableArray *)arr;
- (void) addInArchiveExtension: (NSString*)ext;
- (void) addInArchiveFilePattern: (NSString*)pattern;
- (void) addInDirPattern: (NSString*)pattern;
- (void) addInExtension: (NSString*)ext;
- (void) addInFilePattern: (NSString*)pattern;
- (void) addOutArchiveExtension: (NSString*)ext;
- (void) addOutArchiveFilePattern: (NSString*)pattern;
- (void) addOutDirPattern: (NSString*)pattern;
- (void) addOutExtension: (NSString*)ext;
- (void) addOutFilePattern: (NSString*)pattern;
- (void) addSearchPattern: (NSString*)pattern;

- (void) addFileType:(NSString*)typeName toArr:(NSMutableArray *)arr;
- (void) addInFileType: (NSString*)typeName;
- (void) addOutFileType: (NSString*)typeName;

//- (void) setArchivesOnly: (BOOL)b;

@end
