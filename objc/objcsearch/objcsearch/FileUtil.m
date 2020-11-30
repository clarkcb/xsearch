//
//  FileUtil.m
//  objcsearch
//
//  Created by Cary Clark on 1/14/18.
//  Copyright © 2018 Cary Clark. All rights reserved.
//

#import "FileUtil.h"

@implementation FileUtil

+ (NSSet<NSString*>*) dotDirs {
    return [NSSet setWithObjects:@".", @"..", @"./", @"../", nil];
}

+ (NSString*) expandPath:(NSString*)filePath {
    return [filePath stringByExpandingTildeInPath];
}

+ (NSString*) getExtension:(NSString*)fileName {
    NSURL *fileURL = [NSURL fileURLWithPath:fileName];
    NSString *ext = [[fileURL pathExtension] uppercaseString];
    if ([ext isEqualToString:@"Z"]) {
        return ext;
    }
    return [ext lowercaseString];
}

+ (BOOL) hasExtension:(NSString *)fileName ext:(NSString *)ext {
    return [[self getExtension:fileName] isEqualToString:[ext lowercaseString]];
}

+ (NSFileManager *) getFileManager {
    return [NSFileManager defaultManager];
}

+ (NSArray<NSString*>*) contentsForPath:(NSString*)filePath error:(NSError**)error {
    return [[self getFileManager] contentsOfDirectoryAtPath:filePath error:error];
}

+ (NSDirectoryEnumerationOptions) optionsForSettings:(SearchSettings*)settings {
    NSDirectoryEnumerationOptions options = NSDirectoryEnumerationSkipsPackageDescendants;
    if (settings.excludeHidden) {
        options |= NSDirectoryEnumerationSkipsHiddenFiles;
    }
    if (!settings.recursive) {
        options |= NSDirectoryEnumerationSkipsSubdirectoryDescendants;
    }
    return options;
}

+ (NSDirectoryEnumerator*) enumeratorForPath:(NSString*)filePath settings:(SearchSettings*)settings{
    NSDirectoryEnumerationOptions options = [self optionsForSettings:settings];
    return [[self getFileManager]
            enumeratorAtURL:[NSURL fileURLWithPath:filePath]
            includingPropertiesForKeys:@[NSURLIsDirectoryKey, NSURLIsRegularFileKey]
            options:options
            errorHandler:nil];
}

+ (BOOL) exists:(NSString*)filePath {
    return [[self getFileManager] fileExistsAtPath:filePath];
}

+ (BOOL) isDirectory:(NSString*)filePath {
    if ([[self dotDirs] containsObject:filePath]) {
        return true;
    }
    BOOL isDir;
    if ([[self getFileManager] fileExistsAtPath:filePath isDirectory:&isDir]) {
        return isDir;
    }
    if ([[self getFileManager] fileExistsAtPath:[filePath stringByExpandingTildeInPath] isDirectory:&isDir]) {
        return isDir;
    }
    return false;
}

+ (BOOL) isDotDir:(NSString*)filePath {
    return [[self dotDirs] containsObject:filePath];
}

+ (BOOL) isHidden:(NSString*)filePath {
    NSArray<NSString*> *elems = [filePath componentsSeparatedByString:@"/"];
    for (NSString *elem in elems) {
        if ([self isHiddenFile:elem]) {
            return true;
        }
    }
    return false;
}

+ (BOOL) isHiddenFile:(NSString*)fileName {
    return [fileName hasPrefix:@"."] && ![self isDotDir:fileName];
}

+ (BOOL) isReadableFile:(NSString*)filePath {
    return [[self getFileManager] isReadableFileAtPath:filePath];
}

+ (NSString*) joinPath:(NSString*)path childPath:(NSString*)childPath {
    NSMutableString *joined = [NSMutableString stringWithString:path];
    if (![joined hasSuffix:@"/"]) {
        [joined appendString:@"/"];
    }
    [joined appendString:childPath];
    return [NSString stringWithString:joined];
}

@end
