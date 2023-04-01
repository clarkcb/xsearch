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

+ (BOOL) isDotDir:(NSString*)filePath {
    return [[self dotDirs] containsObject:filePath];
}

+ (NSString*) expandPath:(NSString*)filePath {
    return [filePath stringByExpandingTildeInPath];
}

+ (NSString*) contractPath:(NSString*)filePath {
    return [filePath stringByAbbreviatingWithTildeInPath];
}

+ (NSString*) absolutePath:(NSString*)filePath {
    NSString *normFilePath = [self normalizePath:filePath];
    if ([self isDotDir:normFilePath] || [normFilePath hasPrefix:@"./"] || [normFilePath containsString:@"../"]) {
        NSString *pwd = [[NSFileManager defaultManager] currentDirectoryPath];
        if ([normFilePath isEqualToString:@"."]) {
            return pwd;
        }
        if ([normFilePath isEqualToString:@".."]) {
            return [pwd stringByDeletingLastPathComponent];
        }
        if ([normFilePath hasPrefix:@"./"]) {
            return [pwd stringByAppendingString:[normFilePath substringFromIndex:1]];
        }
        if ([normFilePath containsString:@"../"]) {
            NSArray<NSString*> *elems = [normFilePath componentsSeparatedByString:@"/"];
            NSMutableString *resolved;
            if ([elems[0] isEqualToString:@".."]) {
                resolved = [NSMutableString stringWithString:[pwd stringByDeletingLastPathComponent]];
            } else {
                resolved = [NSMutableString stringWithString:elems[0]];
            }
            for (unsigned long i=1; i < [elems count]; i++) {
                if ([elems[i] isEqualToString:@".."]) {
                    resolved = [NSMutableString stringWithString:[resolved stringByDeletingLastPathComponent]];
                } else if (![elems[i] isEqualToString:@""]) {
                    [resolved appendString:[NSString stringWithFormat:@"/%@", elems[i]]];
                }
            }
            return [NSString stringWithString:resolved];
        }
    }
    return filePath;
}

+ (NSString*) relativePath:(NSString*)filePath to:(NSString*)toPath {
    NSString *normFilePath = [self normalizePath:filePath];
    NSString *normToPath = [self normalizePath:toPath];
    NSString *absToPath = [self absolutePath:normToPath];
    if ([absToPath isEqualToString:normToPath]) {
        return filePath;
    }
    return [normFilePath stringByReplacingOccurrencesOfString:absToPath withString:normToPath];
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

+ (BOOL) allExist:(NSArray<NSString*>*)filePaths {
    for (NSString *fp in filePaths) {
        if (![self exists:fp] && ![self exists:[self expandPath:fp]]) {
            return false;
        }
    }
    return true;
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

+ (BOOL) allReadable:(NSArray<NSString*>*)filePaths {
    for (NSString *fp in filePaths) {
        if (![self isReadableFile:fp] && ![self isReadableFile:[self expandPath:fp]]) {
            return false;
        }
    }
    return true;
}

+ (NSString*) joinPath:(NSString*)path childPath:(NSString*)childPath {
    NSMutableString *joined = [NSMutableString stringWithString:path];
    if (![joined hasSuffix:@"/"]) {
        [joined appendString:@"/"];
    }
    [joined appendString:childPath];
    return [NSString stringWithString:joined];
}

+ (NSString*) normalizePath:(NSString*)path {
    if ([path hasSuffix:@"/"]) {
        return [path substringToIndex:[path length] - 1];
    }
    return path;
}

@end
