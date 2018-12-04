#import "SearchFile.h"

@implementation SearchFile

- (instancetype) initWithFilePath:(NSString *)filePath fileType:(FileType)fileType {
    self = [super init];
    if (self) {
        self.containers = [NSArray array];
        self.filePath = filePath;
        self.fileType = fileType;
    }
    return self;
}

- (NSString *)description {
    NSMutableString *s = [NSMutableString string];
    if ([self.containers count] > 0) {
        [s appendFormat:@"%@!", [self.containers componentsJoinedByString:@"!"]];
    }
    [s appendString:self.filePath];
    return [NSString stringWithString:s];
}

@end
