#import "SearchOption.h"

@implementation SearchOption

- (instancetype) initWithShortArg:(NSString *)sArg withLongArg:(NSString *)lArg withDesc:(NSString *)desc {
    self = [super init];
    if (self) {
        self.shortArg = sArg;
        self.longArg = lArg;
        self.desc = desc;
    }
    return self;
}

- (NSString *) sortArg {
    if (self.shortArg) {
        return [NSString stringWithFormat:@"%@@%@", [self.shortArg lowercaseString], [self.longArg lowercaseString]];
    }
    return [self.longArg lowercaseString];
}

- (NSString *) description {
    NSMutableString *d = [[NSMutableString alloc] initWithString:@"SearchOption("];
    [d appendFormat:@"short: \"%@\", long: \"%@\", desc: \"%@\")",
     self.shortArg, self.longArg, self.description];
    return d;
}

@end
