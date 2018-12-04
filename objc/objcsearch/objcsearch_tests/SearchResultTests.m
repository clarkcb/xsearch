//
//  SearchResultTests.m
//  objcsearch_tests
//
//  Created by Cary Clark on 11/12/18.
//  Copyright © 2018 Cary Clark. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "common.h"
#import "Regex.h"
#import "SearchFile.h"
#import "SearchResult.h"

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
    Regex *regex = [[Regex alloc] initWithPattern:@"Search"];
    SearchFile *sf = [[SearchFile alloc] initWithFilePath:@"~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs" fileType:FileTypeCode];
    long lineNum = 10;
    long matchStartIndex = 15;
    long matchEndIndex = 23;
    NSString *line = @"\tpublic class Searcher\n";

    SearchResult *result = [[SearchResult alloc] initWithPattern:[regex pattern]
                                                            file:sf
                                                         lineNum:lineNum
                                                 matchStartIndex:matchStartIndex
                                                   matchEndIndex:matchEndIndex
                                                            line:line
                                                     linesBefore:[[NSArray alloc] init]
                                                      linesAfter:[[NSArray alloc] init]];

    NSString *trimmedLine = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    NSString *expectedOutput = [NSString stringWithFormat:@"%@: %lu: [%lu:%lu]: %@", [sf description], lineNum,
                                matchStartIndex, matchEndIndex, trimmedLine];

    XCTAssert([[result description] isEqualToString:expectedOutput]);
}

- (void)testBinaryFileSearchResult {
    Regex *regex = [[Regex alloc] initWithPattern:@"Search"];
    SearchFile *sf = [[SearchFile alloc] initWithFilePath:@"~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.exe" fileType:FileTypeBinary];
    long lineNum = 0;
    long matchStartIndex = 0;
    long matchEndIndex = 0;
    
    SearchResult *result = [[SearchResult alloc] initWithPattern:[regex pattern]
                                                            file:sf
                                                         lineNum:lineNum
                                                 matchStartIndex:matchStartIndex
                                                   matchEndIndex:matchEndIndex
                                                            line:NULL
                                                     linesBefore:[[NSArray alloc] init]
                                                      linesAfter:[[NSArray alloc] init]];

    NSString *expectedOutput = [NSString stringWithFormat:@"%@ matches at [%lu:%lu]", [sf description],
                                matchStartIndex, matchEndIndex];
    
    XCTAssert([[result description] isEqualToString:expectedOutput]);
}

- (void)testMultiLineSearchResult {
    Regex *regex = [[Regex alloc] initWithPattern:@"Search"];
    NSString *filePath = @"~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
    SearchFile *sf = [[SearchFile alloc] initWithFilePath:filePath fileType:FileTypeText];
    long lineNum = 10;
    long matchStartIndex = 15;
    long matchEndIndex = 23;
    NSString *line = @"\tpublic class Searcher\n";
    NSArray<NSString*> *linesBefore = @[@"namespace CsSearch\n", @"{\n"];
    NSArray<NSString*> *linesAfter = @[@"\t{\n", @"\t\tprivate readonly FileTypes _fileTypes;\n"];

    SearchResult *result = [[SearchResult alloc] initWithPattern:[regex pattern]
                                                            file:sf
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

    XCTAssert([[result description] isEqualToString:expectedOutput]);
}

@end
