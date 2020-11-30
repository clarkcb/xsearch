#import <XCTest/XCTest.h>
#import "FileTypes.h"
#import "SearchFile.h"

@interface SearchFileTests : XCTestCase
//@property FileTypes *fileTypes;
@end

@implementation SearchFileTests

- (void)testSearchFileAbsPath {
    NSString *path = @"/Users/cary/src/xsearch/objc/objcsearch/objcsearch/SearchFile.m";
    SearchFile *searchFile = [[SearchFile alloc]
                              initWithFilePath:path
                              fileType:FileTypeCode];
    NSString *searchFileString = [searchFile description];
    XCTAssert([searchFileString isEqualToString:path]);
}

- (void)testSearchFileTildePath {
    NSString *path = @"~/src/xsearch/objc/objcsearch/objcsearch/SearchFile.m";
    SearchFile *searchFile = [[SearchFile alloc]
                              initWithFilePath:path
                              fileType:FileTypeCode];
    NSString *searchFileString = [searchFile description];
    XCTAssert([searchFileString isEqualToString:path]);
}

- (void)testSearchFileRelPath1 {
    NSString *path = @"./SearchFile.m";
    SearchFile *searchFile = [[SearchFile alloc]
                              initWithFilePath:path
                              fileType:FileTypeCode];
    NSString *searchFileString = [searchFile description];
    XCTAssert([searchFileString isEqualToString:path]);
}

- (void)testSearchFileRelPath2 {
    NSString *path = @"../SearchFile.m";
    SearchFile *searchFile = [[SearchFile alloc]
                              initWithFilePath:path
                              fileType:FileTypeCode];
    NSString *searchFileString = [searchFile description];
    XCTAssert([searchFileString isEqualToString:path]);
}

@end
