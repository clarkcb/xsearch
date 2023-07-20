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
 * searchMultiLineString tests
 *************************************************************/
- (void)testSearchMultiLineString {
    SearchSettings *settings = [[SearchSettings alloc] init];
    [settings addPath:self.testFilePath];
    [settings addSearchPattern:@"Searcher"];
    NSError *error = nil;
    Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];
    
    NSArray<SearchResult*> *results = [searcher search:&error];

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
