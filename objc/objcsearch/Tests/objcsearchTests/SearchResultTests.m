//
//  SearchResultTests.m
//  objcsearch_tests
//
//  Created by Cary Clark on 11/12/18.
//  Copyright © 2018 Cary Clark. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "common.h"
#import "FileResult.h"
#import "Regex.h"
#import "SearchResult.h"
#import "SearchResultFormatter.h"
#import "SearchSettings.h"

@interface SearchResultTests : XCTestCase

@end

@implementation SearchResultTests

- (void)setUp {
    [super setUp];
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testSingleLineSearchResult {
    SearchSettings *settings = [[SearchSettings alloc] init];
    settings.colorize = false;
    SearchResultFormatter *formatter = [[SearchResultFormatter alloc] initWithSettings:settings];
    Regex *regex = [[Regex alloc] initWithPattern:@"Search"];
    FileResult *fr = [[FileResult alloc] initWithFilePath:@"~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs" fileType:FileTypeCode fileSize:0 lastMod:nil];
    long lineNum = 10;
    long matchStartIndex = 15;
    long matchEndIndex = 23;
    NSString *line = @"\tpublic class Searcher\n";

    SearchResult *result = [[SearchResult alloc] initWithPattern:[regex pattern]
                                                            file:fr
                                                         lineNum:lineNum
                                                 matchStartIndex:matchStartIndex
                                                   matchEndIndex:matchEndIndex
                                                            line:line
                                                     linesBefore:[[NSArray alloc] init]
                                                      linesAfter:[[NSArray alloc] init]];

    NSString *trimmedLine = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    NSString *expectedOutput = [NSString stringWithFormat:@"%@: %lu: [%lu:%lu]: %@", [fr description], lineNum,
                                matchStartIndex, matchEndIndex, trimmedLine];
    NSString *output = [formatter format:result];
    XCTAssert([output isEqualToString:expectedOutput]);
}

- (void)testBinaryFileSearchResult {
    SearchSettings *settings = [[SearchSettings alloc] init];
    SearchResultFormatter *formatter = [[SearchResultFormatter alloc] initWithSettings:settings];
    Regex *regex = [[Regex alloc] initWithPattern:@"Search"];
    FileResult *fr = [[FileResult alloc] initWithFilePath:@"~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe" fileType:FileTypeBinary fileSize:0 lastMod:nil];
    long lineNum = 0;
    long matchStartIndex = 0;
    long matchEndIndex = 0;
    
    SearchResult *result = [[SearchResult alloc] initWithPattern:[regex pattern]
                                                            file:fr
                                                         lineNum:lineNum
                                                 matchStartIndex:matchStartIndex
                                                   matchEndIndex:matchEndIndex
                                                            line:NULL
                                                     linesBefore:[[NSArray alloc] init]
                                                      linesAfter:[[NSArray alloc] init]];

    NSString *expectedOutput = [NSString stringWithFormat:@"%@ matches at [%lu:%lu]", [fr description],
                                matchStartIndex, matchEndIndex];
    NSString *output = [formatter format:result];
    XCTAssert([output isEqualToString:expectedOutput]);
}

- (void)testMultiLineSearchResult {
    SearchSettings *settings = [[SearchSettings alloc] init];
    SearchResultFormatter *formatter = [[SearchResultFormatter alloc] initWithSettings:settings];
    Regex *regex = [[Regex alloc] initWithPattern:@"Search"];
    NSString *filePath = @"~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
    FileResult *fr = [[FileResult alloc] initWithFilePath:filePath fileType:FileTypeText fileSize:0 lastMod:nil];
    long lineNum = 10;
    long matchStartIndex = 15;
    long matchEndIndex = 23;
    NSString *line = @"\tpublic class Searcher\n";
    NSArray<NSString*> *linesBefore = @[@"namespace CsSearch\n", @"{\n"];
    NSArray<NSString*> *linesAfter = @[@"\t{\n", @"\t\tprivate readonly FileTypes _fileTypes;\n"];

    SearchResult *result = [[SearchResult alloc] initWithPattern:[regex pattern]
                                                            file:fr
                                                         lineNum:lineNum
                                                 matchStartIndex:matchStartIndex
                                                   matchEndIndex:matchEndIndex
                                                            line:line
                                                     linesBefore:linesBefore
                                                      linesAfter:linesAfter];
    
    NSString *expectedOutput = [NSString stringWithFormat:@"================================================================================\n"
    "%@: %lu: [%lu:%lu]\n"
    "--------------------------------------------------------------------------------\n"
    "   8 | namespace CsSearch\n"
    "   9 | {\n"
    "> 10 | \tpublic class Searcher\n"
    "  11 | \t{\n"
    "  12 | \t\tprivate readonly FileTypes _fileTypes;\n", filePath, lineNum, matchStartIndex, matchEndIndex];

    NSString *output = [formatter format:result];
    logMsg(@"\noutput:");
    logMsg(output);
    logMsg(@"\nexpectedOutput:");
    logMsg(expectedOutput);
    XCTAssert([output isEqualToString:expectedOutput]);
}

@end
