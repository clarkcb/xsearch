#import <Foundation/Foundation.h>
#import "FileTypes.h"
#import "SearchFile.h"
#import "SearchResult.h"
#import "SearchSettings.h"

@interface Searcher : NSObject

@property FileTypes *fileTypes;
@property SearchSettings *settings;

- (instancetype) initWithSettings:(SearchSettings*)settings error:(NSError**)error;
- (NSArray<SearchResult*>*) search:(NSError**)error;
- (NSArray<SearchResult*>*) searchDirPath:(NSString*)filePath error:(NSError**)error;
- (NSArray<SearchResult*>*) searchFilePath:(NSString*)filePath error:(NSError**)error;
- (NSArray<SearchResult*>*) searchFile:(SearchFile*)sf error:(NSError**)error;
- (NSArray<SearchResult*>*) searchMultiLineString:(NSString*)s error:(NSError**)error;

// private methods
//- (NSArray<NSString*>*) getSearchDirs:(NSString*)filePath;
//- (NSArray<SearchFile*>*) getSearchFiles:(NSArray<NSString*>*)searchDirs;
//- (NSArray<SearchFile*>*) getSearchFilesForDirectory:(NSString*)dirPath error:(NSError**)error;
- (BOOL) filterFile:(NSString*)filePath;
- (BOOL) isArchiveSearchFile:(NSString*)filePath;
- (BOOL) isSearchDir:(NSString*)dirPath;
- (BOOL) isSearchFile:(NSString*)filePath;

@end
