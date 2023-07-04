//
//  SearchSettingsTests.m
//  objcsearch_tests
//
//  Created by Cary Clark on 11/12/18.
//  Copyright © 2018 Cary Clark. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "SearchSettings.h"

@interface SearchSettingsTests : XCTestCase

@end

@implementation SearchSettingsTests

- (void)setUp {
    [super setUp];
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testDefaultSettings {
    SearchSettings *settings = [[SearchSettings alloc] init];
    XCTAssert(![settings archivesOnly]);
    XCTAssert([settings colorize]);
    XCTAssert(![settings debug]);
    XCTAssert([settings excludeHidden]);
    XCTAssert(![settings firstMatch]);
    XCTAssert(![settings listDirs]);
    XCTAssert(![settings listFiles]);
    XCTAssert(![settings listLines]);
    XCTAssert(![settings multiLineSearch]);
    XCTAssert([settings printResults]);
    XCTAssert(![settings printUsage]);
    XCTAssert(![settings printVersion]);
    XCTAssert(![settings searchArchives]);
    XCTAssert(![settings uniqueLines]);
    XCTAssert(![settings verbose]);
}

- (void)testAddExtensions {
    SearchSettings *settings = [[SearchSettings alloc] init];
    XCTAssert([[settings inExtensions] count] == 0);
    [settings addInExtension:@"java,scala"];
    XCTAssert([[settings inExtensions] count] == 2);
    XCTAssert([[[settings inExtensions] objectAtIndex:0] isEqual:@"java"]);
    XCTAssert([[[settings inExtensions] objectAtIndex:1] isEqual:@"scala"]);
}

- (void)testAddPattern {
    SearchSettings *settings = [[SearchSettings alloc] init];
    XCTAssert([[settings searchPatterns] count] == 0);
    [settings addSearchPattern:@"Searcher"];
    XCTAssert([[settings searchPatterns] count] == 1);
    XCTAssert([[[[settings searchPatterns] objectAtIndex:0] pattern] isEqual:@"Searcher"]);
}

- (void)testSetArchivesOnly {
    SearchSettings *settings = [[SearchSettings alloc] init];
    XCTAssert(![settings archivesOnly]);
    XCTAssert(![settings searchArchives]);
    [settings setArchivesOnly:true];
    XCTAssert([settings archivesOnly]);
    XCTAssert([settings searchArchives]);
}

- (void)testSetDebug {
    SearchSettings *settings = [[SearchSettings alloc] init];
    XCTAssert(![settings debug]);
    XCTAssert(![settings verbose]);
    [settings setDebug:true];
    XCTAssert([settings debug]);
    XCTAssert([settings verbose]);
}

@end
