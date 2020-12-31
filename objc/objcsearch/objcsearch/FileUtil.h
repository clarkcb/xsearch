//
//  FileUtil.h
//  objcsearch
//
//  Created by Cary Clark on 1/14/18.
//  Copyright © 2018 Cary Clark. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "config.h"
#import "SearchSettings.h"

@interface FileUtil : NSObject

+ (NSSet<NSString*>*) dotDirs;
+ (NSString*) expandPath:(NSString*)filePath;
+ (NSString*) contractPath:(NSString*)filePath;
+ (NSString*) absolutePath:(NSString*)filePath;
+ (NSString*) relativePath:(NSString*)filePath to:(NSString*)toPath;
+ (NSString*) getExtension:(NSString*)fileName;
+ (BOOL) hasExtension:(NSString*)fileName ext:(NSString*)ext;
+ (NSArray<NSString*>*) contentsForPath:(NSString*)filePath error:(NSError**)error;
+ (NSDirectoryEnumerator*) enumeratorForPath:(NSString*)filePath settings:(SearchSettings*)settings;
+ (BOOL) exists:(NSString*)filePath;
+ (BOOL) isDirectory:(NSString*)filePath;
+ (BOOL) isDotDir:(NSString*)filePath;
+ (BOOL) isHidden:(NSString*)filePath;
+ (BOOL) isHiddenFile:(NSString*)fileName;
+ (BOOL) isReadableFile:(NSString*)filePath;
+ (NSString*) joinPath:(NSString*)path childPath:(NSString*)childPath;
+ (NSString*) normalizePath:(NSString*)path;
+ (NSDirectoryEnumerationOptions) optionsForSettings:(SearchSettings*)settings;

@end
