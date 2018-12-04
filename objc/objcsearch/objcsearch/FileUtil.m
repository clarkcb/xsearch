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
    if ([filePath length] > 0 && [filePath characterAtIndex:0] == '~') {
        NSString* homePath = [[[NSProcessInfo processInfo]environment]objectForKey:@"HOME"];
        NSMutableString *expanded = [NSMutableString stringWithString:homePath];
        if ([filePath length] > 1) {
            [expanded appendString:[filePath substringFromIndex:1]];
        }
        return [NSString stringWithString:expanded];
    }
    return filePath;
}

+ (NSString*) getExtension:(NSString*)fileName {
    NSRange range = [fileName rangeOfString:@"." options:NSBackwardsSearch];
    if (range.location != NSNotFound && range.location > 0 && range.location <= [fileName length] - 1) {
        return [fileName substringFromIndex:range.location + 1];
    }
    return @"";
}

+ (BOOL) hasExtension:(NSString *)fileName ext:(NSString *)ext {
    return [[self getExtension:fileName] isEqualToString:ext];
}

+ (NSFileManager *) getFileManager {
    return [NSFileManager defaultManager];
}

+ (NSArray<NSString*>*) contentsForPath:(NSString*)filePath error:(NSError**)error {
    return [[self getFileManager] contentsOfDirectoryAtPath:filePath error:error];
}

+ (NSDirectoryEnumerator*) enumeratorForPath:(NSString*)filePath {
    return [[self getFileManager] enumeratorAtPath:filePath];
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
