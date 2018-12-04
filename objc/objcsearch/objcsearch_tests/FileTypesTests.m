#import <XCTest/XCTest.h>
#import "FileTypes.h"

@interface FileTypesTests : XCTestCase
@property FileTypes *fileTypes;
@end

@implementation FileTypesTests

- (void)setUp {
    [super setUp];
    self.fileTypes = [[FileTypes alloc] init];
}

- (void)tearDown {
    [super tearDown];
}

- (void)testArchiveFiles {
    NSArray<NSString*> *exts = @[@"7z", @"arj", @"bz2", @"cpio", @"ear", @"gz", @"hqx", @"jar",
                                 @"pax", @"rar", @"sit", @"sitx", @"tar", @"tgz", @"war", @"zip",
                                 @"zipx", @"Z"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"archive.%@", x];
        XCTAssert([self.fileTypes getFileType:fileName] == FileTypeArchive);
        XCTAssert([self.fileTypes isArchiveFile:fileName]);
        XCTAssert(![self.fileTypes isBinaryFile:fileName]);
        XCTAssert(![self.fileTypes isCodeFile:fileName]);
    }
}

- (void)testBinaryFiles {
    NSArray<NSString*> *exts = @[@"a", @"ai", @"beam", @"bin", @"chm", @"class", @"com", @"dat",
                                 @"dll", @"doc", @"dot", @"dylib", @"exe", @"hlp", @"lib", @"mdb",
                                 @"pdb", @"so"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"binary.%@", x];
        XCTAssert([self.fileTypes getFileType:fileName] == FileTypeBinary);
        XCTAssert(![self.fileTypes isArchiveFile:fileName]);
        XCTAssert([self.fileTypes isBinaryFile:fileName]);
        XCTAssert(![self.fileTypes isCodeFile:fileName]);
    }
}

- (void)testCodeFiles {
    NSArray<NSString*> *exts = @[@"bas", @"bash", @"bat", @"c", @"clj", @"cpp", @"cs", @"erl",
                                 @"fs", @"go", @"groovy", @"h", @"hpp", @"hs", @"java", @"js",
                                 @"m", @"php", @"py", @"rb", @"scala", @"sh",  @"swift", @"ts",
                                 @"vb"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"code.%@", x];
        XCTAssert([self.fileTypes getFileType:fileName] == FileTypeCode);
        XCTAssert(![self.fileTypes isArchiveFile:fileName]);
        XCTAssert(![self.fileTypes isBinaryFile:fileName]);
        XCTAssert([self.fileTypes isCodeFile:fileName]);
    }
}

- (void)testTextFiles {
    NSArray<NSString*> *exts = @[@"asp", @"cfg", @"cmake", @"conf", @"css", @"csv", @"ddl", @"dtd",
                                 @"htm", @"html", @"ics", @"ini", @"log", @"markdown", @"md", @"po",
                                 @"pot", @"properties", @"rc", @"rtf", @"scc", @"sgm",  @"sgml",
                                 @"sql", @"txt", @"yml"];
    for (NSString *x in exts) {
        NSString *fileName = [NSString stringWithFormat:@"text.%@", x];
         XCTAssert([self.fileTypes getFileType:fileName] == FileTypeText);
        XCTAssert(![self.fileTypes isArchiveFile:fileName]);
        XCTAssert(![self.fileTypes isBinaryFile:fileName]);
        XCTAssert(![self.fileTypes isCodeFile:fileName]);
        XCTAssert([self.fileTypes isTextFile:fileName]);
    }
}

@end
