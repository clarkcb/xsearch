//
//  SearcherTest.m
//  objcsearch_tests
//
//  Created by Cary Clark on 11/10/18.
//  Copyright © 2018 Cary Clark. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "FileUtil.h"
#import "Searcher.h"
#import "SearchSettings.h"

@interface SearcherTests : XCTestCase
@property NSString *testFilePath;
@end

@implementation SearcherTests

- (void)setUp {
    [super setUp];
    self.testFilePath = [FileUtil joinPath:[NSString stringWithUTF8String:SHAREDPATH] childPath:@"testFiles/testFile2.txt"];
}

- (void)tearDown {
    [super tearDown];
}

/*************************************************************
 * isSearchDir tests
 *************************************************************/
- (void)testIsSearchDir_SingleDot_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isSearchDir:@"."]);
}

- (void)testIsSearchDir_DoubleDot_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isSearchDir:@".."]);
}

- (void)testIsSearchDir_IsHidden_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher isSearchDir:@".git"]);
}

- (void)testIsSearchDir_IsHiddenIncludeHidden_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings setExcludeHidden:false];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isSearchDir:@".git"]);
}

- (void)testIsSearchDir_NoPatterns_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isSearchDir:@"/Users"]);
}

- (void)testIsSearchDir_MatchesInPattern_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addInDirPattern:@"Search"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isSearchDir:@"CsSearch"]);
}

- (void)testIsSearchDir_MatchesOutPattern_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addOutDirPattern:@"Search"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher isSearchDir:@"CsSearch"]);
}

- (void)testIsSearchDir_DoesNotMatchOutPattern_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addOutDirPattern:@"SearchFiles"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isSearchDir:@"CsSearch"]);
}

/*************************************************************
 * isSearchFile tests
 *************************************************************/
- (void)testIsSearchFile_NoExtensionsNoPatterns_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isSearchFile:@"FileUtil.cs"]);
}

- (void)testIsSearchFile_MatchesInExtension_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addInExtension:@"cs"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isSearchFile:@"FileUtil.cs"]);
}

- (void)testIsSearchFile_DoesNotMatchInExtension_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addInExtension:@"java"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher isSearchFile:@"FileUtil.cs"]);
}

- (void)testIsSearchFile_MatchesOutExtension_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addOutExtension:@"cs"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher isSearchFile:@"FileUtil.cs"]);
}

- (void)testIsSearchFile_DoesNotMatchOutExtension_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addOutExtension:@"java"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isSearchFile:@"FileUtil.cs"]);
}

- (void)testIsSearchFile_MatchesInFilePattern_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addInFilePattern:@"Search"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isSearchFile:@"Searcher.cs"]);
}

- (void)testIsSearchFile_DoesNotMatchInFilePattern_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addInFilePattern:@"Search"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher isSearchFile:@"FileUtil.cs"]);
}

- (void)testIsSearchFile_MatchesOutFilePattern_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addOutFilePattern:@"Search"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher isSearchFile:@"Searcher.cs"]);
}

- (void)testIsSearchFile_DoesNotMatchOutFilePattern_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addOutFilePattern:@"Search"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isSearchFile:@"FileUtil.cs"]);
}

/*************************************************************
 * isArchiveSearchFile tests
 *************************************************************/
- (void)testIsArchiveSearchFile_NoExtensionsNoPatterns_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isArchiveSearchFile:@"archive.zip"]);
}

- (void)testIsArchiveSearchFile_MatchesInExtension_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addInArchiveExtension:@"zip"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isArchiveSearchFile:@"archive.zip"]);
}

- (void)testIsArchiveSearchFile_DoesNotMatchInExtension_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addInArchiveExtension:@"gz"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher isArchiveSearchFile:@"archive.zip"]);
}

- (void)testIsArchiveSearchFile_MatchesOutExtension_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addOutArchiveExtension:@"zip"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher isArchiveSearchFile:@"archive.zip"]);
}

- (void)testIsArchiveSearchFile_DoesNotMatchOutExtension_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addOutArchiveExtension:@"gz"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isArchiveSearchFile:@"archive.zip"]);
}

- (void)testIsArchiveSearchFile_MatchesInArchiveFilePattern_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addInArchiveFilePattern:@"arch"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isArchiveSearchFile:@"archive.zip"]);
}

- (void)testIsArchiveSearchFile_DoesNotMatchInArchiveFilePattern_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addInArchiveFilePattern:@"archives"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher isArchiveSearchFile:@"archive.zip"]);
}

- (void)testIsArchiveSearchFile_MatchesOutArchiveFilePattern_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addOutArchiveFilePattern:@"arch"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher isArchiveSearchFile:@"archive.zip"]);
}

- (void)testIsArchiveSearchFile_DoesNotMatchOutArchiveFilePattern_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addOutArchiveFilePattern:@"archives"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher isArchiveSearchFile:@"archive.zip"]);
}

/*************************************************************
 * FilterFile tests
 *************************************************************/
- (void)testFilterFile_IsHidden_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher filterFile:@".gitignore"]);
}

- (void)testFilterFile_IsHiddenIncludeHidden_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings setExcludeHidden:false];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher filterFile:@".gitignore"]);
}

- (void)testFilterFile_ArchiveNoSearchArchives_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher filterFile:@"archive.zip"]);
}

- (void)testFilterFile_ArchiveSearchArchives_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings setSearchArchives:true];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher filterFile:@"archive.zip"]);
}

- (void)testFilterFile_IsArchiveSearchFile_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings setSearchArchives:true];
    [settings addInArchiveExtension:@"zip"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher filterFile:@"archive.zip"]);
}

- (void)testFilterFile_NotIsArchiveSearchFile_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings setSearchArchives:true];
    [settings addOutArchiveExtension:@"zip"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher filterFile:@"archive.zip"]);
}

- (void)testFilterFile_ArchiveFileArchivesOnly_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings setArchivesOnly:true];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher filterFile:@"archive.zip"]);
}

- (void)testFilterFile_NoExtensionsNoPatterns_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher filterFile:@"FileUtil.cs"]);
}

- (void)testFilterFile_IsSearchFile_True {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addInExtension:@"cs"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert([searcher filterFile:@"FileUtil.cs"]);
}

- (void)testFilterFile_NotIsSearchFile_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addOutExtension:@"cs"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher filterFile:@"FileUtil.cs"]);
}

- (void)testFilterFile_NonArchiveFileArchivesOnly_False {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings setArchivesOnly:true];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    XCTAssert(![searcher filterFile:@"FileUtil.cs"]);
}

/*************************************************************
 * searchMultiLineString tests
 *************************************************************/
- (void)testSearchMultiLineString {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addPath:@"."];
    [settings addSearchPattern:@"Searcher"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    
    NSArray<SearchResult*> *results = [searcher searchFilePath:self.testFilePath error:&error];

    XCTAssert([results count] == 2);

    SearchResult *firstResult = [results objectAtIndex:0];
    int expectedFirstLineNum = 29;
    XCTAssert([firstResult lineNum] == expectedFirstLineNum);
    int expectedFirstMatchStartIndex = 3;
    XCTAssert([firstResult matchStartIndex] == expectedFirstMatchStartIndex);
    int expectedFirstMatchEndIndex = 11;
    XCTAssert([firstResult matchEndIndex] == expectedFirstMatchEndIndex);

    SearchResult *secondResult = [results objectAtIndex:1];
    int expectedSecondLineNum = 35;
    XCTAssert([secondResult lineNum] == expectedSecondLineNum);
    int expectedSecondMatchStartIndex = 24;
    XCTAssert([secondResult matchStartIndex] == expectedSecondMatchStartIndex);
    int expectedSecondMatchEndIndex = 32;
    XCTAssert([secondResult matchEndIndex] == expectedSecondMatchEndIndex);
}

@end
