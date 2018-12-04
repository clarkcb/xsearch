//
//  FileUtilTests.m
//  objcsearch
//
//  Created by Cary Clark on 1/25/18.
//  Copyright © 2018 Cary Clark. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "FileUtil.h"

@interface FileUtilTests : XCTestCase
@property NSString *homePath;
@end

@implementation FileUtilTests

- (void)setUp {
    [super setUp];
    self.homePath = [[[NSProcessInfo processInfo]environment]objectForKey:@"HOME"];
}

- (void)tearDown {
    [super tearDown];
}

/***************************************************************************
 * expandPath tests
 **************************************************************************/
- (void)testExpandPathHasTilde {
    NSString *expected = [FileUtil joinPath:self.homePath childPath:@"filename.txt"];
    XCTAssert([[FileUtil expandPath:@"~/filename.txt"] isEqualToString:expected]);
}

- (void)testExpandPathNoTilde {
    NSString *expected = @"/path/to/filename.txt";
    XCTAssert([[FileUtil expandPath:@"/path/to/filename.txt"] isEqualToString:expected]);
}

/***************************************************************************
 * exist tests
 **************************************************************************/
- (void)testExistsExistingFile {
    NSString *fileTypesFile = [FileUtil joinPath:[NSString stringWithUTF8String:SHAREDPATH] childPath:@"filetypes.json"];
    XCTAssert([FileUtil exists:fileTypesFile]);
}

- (void)testExistsNonexistingFile {
    NSString *fileTypesFile = [FileUtil joinPath:[NSString stringWithUTF8String:SHAREDPATH] childPath:@"filetypes.ZZZ"];
    XCTAssert(![FileUtil exists:fileTypesFile]);
}

/***************************************************************************
 * getExtension tests
 **************************************************************************/
- (void)testGetTxtExtension {
    XCTAssert([[FileUtil getExtension:@"filename.txt"] isEqualToString:@"txt"]);
}

- (void)testGetMissingExtension {
    XCTAssert([[FileUtil getExtension:@"filename."] isEqualToString:@""]);
}

- (void)testGetNoExtension {
    XCTAssert([[FileUtil getExtension:@"filename"] isEqualToString:@""]);
}

- (void)testGetHiddenTxtExtension {
    XCTAssert([[FileUtil getExtension:@".filename.txt"] isEqualToString:@"txt"]);
}

- (void)testGetHiddenMissingExtension {
    XCTAssert([[FileUtil getExtension:@".filename."] isEqualToString:@""]);
}

- (void)testGetHiddenNoExtension {
    XCTAssert([[FileUtil getExtension:@".filename"] isEqualToString:@""]);
}

/***************************************************************************
 * hasExtension tests
 **************************************************************************/
- (void)testHasTxtExtension {
    XCTAssert([FileUtil hasExtension:@"filename.txt" ext:@"txt"]);
}

/***************************************************************************
 * isDirectory tests
 **************************************************************************/
- (void)testIsDirectorySingleDot {
    XCTAssert([FileUtil isDirectory:@"."]);
}

- (void)testIsDirectorySingleDotSlash {
    XCTAssert([FileUtil isDirectory:@"./"]);
}

- (void)testIsDirectoryDoubleDot {
    XCTAssert([FileUtil isDirectory:@".."]);
}

- (void)testIsDirectoryDoubleDotSlash {
    XCTAssert([FileUtil isDirectory:@"../"]);
}

- (void)testIsDirectoryRootDir {
    XCTAssert([FileUtil isDirectory:@"/"]);
}

- (void)testIsDirectoryNonDirectory {
    XCTAssert(![FileUtil isDirectory:@"filename.txt"]);
}

/***************************************************************************
 * isDotDir tests
 **************************************************************************/
- (void)testIsDotDirSingleDot {
    XCTAssert([FileUtil isDotDir:@"."]);
}

- (void)testIsDotDirDoubleDot {
    XCTAssert([FileUtil isDotDir:@".."]);
}

- (void)testIsDotDirSingleDotSlash {
    XCTAssert([FileUtil isDotDir:@"./"]);
}

- (void)testIsDotDirDoubleDotSlash {
    XCTAssert([FileUtil isDotDir:@"../"]);
}

- (void)testIsDotDirNotDotDir {
    XCTAssert(![FileUtil isDotDir:@"~/path"]);
}

- (void)testIsDotDirPathWithSingleDot {
    XCTAssert(![FileUtil isDotDir:@"./path"]);
}

- (void)testIsDotDirPathWithDoubleDot {
    XCTAssert(![FileUtil isDotDir:@"../path"]);
}

- (void)testIsDotDirHiddenFile {
    XCTAssert(![FileUtil isDotDir:@".gitignore"]);
}

/***************************************************************************
 * isHidden tests
 **************************************************************************/
- (void)testIsHiddenSingleDot {
    XCTAssert(![FileUtil isHidden:@"."]);
}

- (void)testIsHiddenDoubleDot {
    XCTAssert(![FileUtil isHidden:@".."]);
}

- (void)testIsHiddenSingleDotSlash {
    XCTAssert(![FileUtil isHidden:@"./"]);
}

- (void)testIsHiddenDoubleDotSlash {
    XCTAssert(![FileUtil isHidden:@"../"]);
}

- (void)testIsHiddenHiddenFile {
    XCTAssert([FileUtil isHidden:@".gitignore"]);
}

- (void)testIsHiddenNotHiddenFile {
    XCTAssert(![FileUtil isHidden:@"filename.txt"]);
}

- (void)testIsHiddenHiddenFilePath {
    XCTAssert([FileUtil isHidden:@"./.gitignore"]);
}

- (void)testIsHiddenNotHiddenFilePath {
    XCTAssert(![FileUtil isHidden:@"./filename.txt"]);
}

/***************************************************************************
 * joinPath tests
 **************************************************************************/
- (void)testJoinPathEndingSlash {
    XCTAssert([[FileUtil joinPath:@"/path/to/" childPath:@"filename.txt"] isEqualToString:@"/path/to/filename.txt"]);
}

- (void)testJoinPathNoEndingSlash {
    XCTAssert([[FileUtil joinPath:@"/path/to" childPath:@"filename.txt"] isEqualToString:@"/path/to/filename.txt"]);
}

@end
