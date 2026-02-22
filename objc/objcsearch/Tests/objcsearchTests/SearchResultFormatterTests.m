//
//  SearchResultFormatterTests.m
//  objcsearch_tests
//
//  Created by Cary Clark on 06/122/25.
//  Copyright © 2025 Cary Clark. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "common.h"
#import "FileResult.h"
#import "Regex.h"
#import "SearchResult.h"
#import "SearchResultFormatter.h"
#import "SearchSettings.h"

@interface SearchResultFormatterTests : XCTestCase

@end

@implementation SearchResultFormatterTests

- (void)setUp {
    [super setUp];
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testSingleLineSearchResultLongerThanMaxLineLength {
    NSString *pattern = @"maxlen";
    NSString *filePath = @"./maxlen.txt";
    FileResult *fr = [[FileResult alloc] initWithFilePath:filePath fileType:FileTypeCode fileSize:0 lastMod:nil];
    long lineNum = 1;
    long matchStartIndex = 53;
    long matchEndIndex = 59;
    NSString *line = @"0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";

    SearchResult *result = [[SearchResult alloc] initWithPattern:pattern
                                                            file:fr
                                                         lineNum:lineNum
                                                 matchStartIndex:matchStartIndex
                                                   matchEndIndex:matchEndIndex
                                                            line:line
                                                     linesBefore:[[NSArray alloc] init]
                                                      linesAfter:[[NSArray alloc] init]];

    NSString *expectedLine = @"...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...";
    NSString *expectedOutput = [NSString stringWithFormat:@"%@: %lu: [%lu:%lu]: %@", [fr description], lineNum,
                                matchStartIndex, matchEndIndex, expectedLine];

    SearchSettings *settings = [[SearchSettings alloc] init];
    settings.colorize = false;
    settings.maxLineLength = 100;
    SearchResultFormatter *formatter = [[SearchResultFormatter alloc] initWithSettings:settings];

    NSString *output = [formatter format:result];
    XCTAssert([output isEqualToString:expectedOutput]);
}

- (void)testSingleLineSearchResultLongerColorize {
    NSString *pattern = @"maxlen";
    NSString *filePath = @"./maxlen.txt";
    FileResult *fr = [[FileResult alloc] initWithFilePath:filePath fileType:FileTypeCode fileSize:0 lastMod:nil];
    long lineNum = 1;
    long matchStartIndex = 53;
    long matchEndIndex = 59;
    NSString *line = @"0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";

    SearchResult *result = [[SearchResult alloc] initWithPattern:pattern
                                                            file:fr
                                                         lineNum:lineNum
                                                 matchStartIndex:matchStartIndex
                                                   matchEndIndex:matchEndIndex
                                                            line:line
                                                     linesBefore:[[NSArray alloc] init]
                                                      linesAfter:[[NSArray alloc] init]];

    NSString *expectedLine = @"...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...";

    SearchSettings *settings = [[SearchSettings alloc] init];
    settings.colorize = true;
    settings.maxLineLength = 100;
    SearchResultFormatter *formatter = [[SearchResultFormatter alloc] initWithSettings:settings];

    NSString *colorizedLine = [formatter colorize:expectedLine matchStartIndex:47 matchEndIndex:53 color:[settings lineColor]];

    NSString *expectedOutput = [NSString stringWithFormat:@"%@: %lu: [%lu:%lu]: %@", [fr description], lineNum,
                                matchStartIndex, matchEndIndex, colorizedLine];

    NSString *output = [formatter format:result];
    XCTAssert([output isEqualToString:expectedOutput]);
}

- (void)testSearchResultMatchLongerColorize {
    NSString *pattern = @"\\d+maxlen\\d+";
    NSString *filePath = @"./maxlen.txt";
    FileResult *fr = [[FileResult alloc] initWithFilePath:filePath fileType:FileTypeCode fileSize:0 lastMod:nil];
    long lineNum = 1;
    long matchStartIndex = 1;
    long matchEndIndex = 110;
    NSString *line = @"0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";

    SearchResult *result = [[SearchResult alloc] initWithPattern:pattern
                                                            file:fr
                                                         lineNum:lineNum
                                                 matchStartIndex:matchStartIndex
                                                   matchEndIndex:matchEndIndex
                                                            line:line
                                                     linesBefore:[[NSArray alloc] init]
                                                      linesAfter:[[NSArray alloc] init]];

    NSString *expectedLine = @"0123456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456...";

    SearchSettings *settings = [[SearchSettings alloc] init];
    settings.colorize = true;
    settings.maxLineLength = 100;
    SearchResultFormatter *formatter = [[SearchResultFormatter alloc] initWithSettings:settings];

    NSString *colorizedLine = [formatter colorize:expectedLine matchStartIndex:0 matchEndIndex:97 color:[settings lineColor]];

    NSString *expectedOutput = [NSString stringWithFormat:@"%@: %lu: [%lu:%lu]: %@", [fr description], lineNum,
                                matchStartIndex, matchEndIndex, colorizedLine];

    NSString *output = [formatter format:result];
    XCTAssert([output isEqualToString:expectedOutput]);
}

- (void)testSearchResult2MatchLongerColorize {
    NSString *pattern = @"\\d+maxlen\\d+";
    NSString *filePath = @"./maxlen.txt";
    FileResult *fr = [[FileResult alloc] initWithFilePath:filePath fileType:FileTypeCode fileSize:0 lastMod:nil];
    long lineNum = 1;
    long matchStartIndex = 11;
    long matchEndIndex = 120;
    NSString *line = @"ABCDEFGHIJ0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789ABCDEFGHIJ";

    SearchResult *result = [[SearchResult alloc] initWithPattern:pattern
                                                            file:fr
                                                         lineNum:lineNum
                                                 matchStartIndex:matchStartIndex
                                                   matchEndIndex:matchEndIndex
                                                            line:line
                                                     linesBefore:[[NSArray alloc] init]
                                                      linesAfter:[[NSArray alloc] init]];

    NSString *expectedLine = @"...3456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456...";

    SearchSettings *settings = [[SearchSettings alloc] init];
    settings.colorize = true;
    settings.maxLineLength = 100;
    SearchResultFormatter *formatter = [[SearchResultFormatter alloc] initWithSettings:settings];

    NSString *colorizedLine = [formatter colorize:expectedLine matchStartIndex:3 matchEndIndex:97 color:[settings lineColor]];

    NSString *expectedOutput = [NSString stringWithFormat:@"%@: %lu: [%lu:%lu]: %@", [fr description], lineNum,
                                matchStartIndex, matchEndIndex, colorizedLine];

    NSString *output = [formatter format:result];
    XCTAssert([output isEqualToString:expectedOutput]);
}

@end
