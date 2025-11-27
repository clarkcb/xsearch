#import "SearchOption.h"

@implementation SearchOption

- (instancetype) initWithShortArg:(NSString *)sArg withLongArg:(NSString *)lArg withDesc:(NSString *)desc withArgType:(ArgTokenType)argType {
    self = [super init];
    if (self) {
        self.shortArg = sArg;
        self.longArg = lArg;
        self.desc = desc;
        self.argType = argType;
    }
    return self;
}

- (NSString *) sortArg {
    if (self.shortArg) {
        return [NSString stringWithFormat:@"%@a%@", [self.shortArg lowercaseString], [self.longArg lowercaseString]];
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
