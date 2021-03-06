//
//  SearchOptionsTests.m
//  objcsearch_tests
//
//  Created by Cary Clark on 11/11/18.
//  Copyright © 2018 Cary Clark. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "FileUtil.h"
#import "SearchOptions.h"

@interface SearchOptionsTests : XCTestCase

@end

@implementation SearchOptionsTests

- (void)setUp {
    [super setUp];
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testSettingsFromMinimalArgs {
    SearchOptions *options = [[SearchOptions alloc] init];
    NSArray *args =[NSArray arrayWithObjects:@"objsearch",@"-s",@"Searcher",@".",nil];
    NSError *error = nil;
    SearchSettings *settings = [options settingsFromArgs:args error:&error];
    XCTAssert(![settings archivesOnly]);
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
    
    XCTAssert([[settings searchPatterns] count] == 1);
    XCTAssert([[[[settings searchPatterns] objectAtIndex:0] pattern] isEqual:@"Searcher"]);
    XCTAssert([[settings startPath] isEqual:@"."]);
}

- (void)testSettingsFromValidArgs {
    SearchOptions *options = [[SearchOptions alloc] init];
    NSArray *args =[NSArray arrayWithObjects:@"objsearch",@"-x",@"java,scala",@"-s",@"Search",@".",nil];
    NSError *error = nil;
    SearchSettings *settings = [options settingsFromArgs:args error:&error];
    
    XCTAssert([[settings inExtensions] count] == 2);
    XCTAssert([[[settings inExtensions] objectAtIndex:0] isEqual:@"java"]);
    XCTAssert([[[settings inExtensions] objectAtIndex:1] isEqual:@"scala"]);
    XCTAssert([[settings searchPatterns] count] == 1);
    XCTAssert([[[[settings searchPatterns] objectAtIndex:0] pattern] isEqual:@"Search"]);
    XCTAssert([[settings startPath] isEqual:@"."]);
}

- (void)testSettingsFromJson {
    SearchSettings *settings = [[SearchSettings alloc] init];

    NSString *startPath = @"~/src/xsearch";
    NSString *json = [NSString stringWithFormat:@"{\n"
                      "\"startpath\": \"%@\",\n"
                      "\"in-ext\": [\"js\", \"ts\"],\n"
                      "\"out-dirpattern\": \"node_module\",\n"
                      "\"out-filepattern\": [\"temp\"],\n"
                      "\"searchpattern\": \"Searcher\",\n"
                      "\"linesbefore\": 2,\n"
                      "\"linesafter\": 2,\n"
                      "\"debug\": true,\n"
                      "\"allmatches\": false,\n"
                      "\"includehidden\": false\n"
                      "}", startPath];

    NSData *data = [json dataUsingEncoding:NSUTF8StringEncoding];

    SearchOptions *options = [[SearchOptions alloc] init];
    [options settingsFromData:data settings:settings];

    XCTAssert([[settings inExtensions] count] == 2);
    XCTAssert([[[settings inExtensions] objectAtIndex:0] isEqual:@"js"]);
    XCTAssert([[[settings inExtensions] objectAtIndex:1] isEqual:@"ts"]);
    XCTAssert([[settings outDirPatterns] count] == 1);
    XCTAssert([[[[settings outDirPatterns] objectAtIndex:0] pattern] isEqual:@"node_module"]);
    XCTAssert([[settings outFilePatterns] count] == 1);
    XCTAssert([[[[settings outFilePatterns] objectAtIndex:0] pattern] isEqual:@"temp"]);
    XCTAssert([[settings searchPatterns] count] == 1);
    XCTAssert([[[[settings searchPatterns] objectAtIndex:0] pattern] isEqual:@"Searcher"]);
    XCTAssert([settings linesBefore] == 2);
    XCTAssert([settings linesAfter] == 2);
    XCTAssert([settings debug]);
    XCTAssert([settings firstMatch]);
    XCTAssert([settings excludeHidden]);
    //NSString *sp = [settings startPath];
    //XCTAssert([[settings startPath] isEqual:startPath]);
}

@end
