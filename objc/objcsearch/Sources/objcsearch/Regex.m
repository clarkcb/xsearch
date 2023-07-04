#import "Regex.h"

@implementation Regex

- (instancetype) initWithPattern:(NSString *)pattern {
    self = [super init];
    if (self) {
        self.pattern = pattern;
        NSError *error = NULL;
        self.expression = [NSRegularExpression
                           regularExpressionWithPattern:self.pattern
                           options:NSRegularExpressionDotMatchesLineSeparators
                           error:&error];
    }
    return self;
}

- (NSArray<NSTextCheckingResult*>*) matches:(NSString*) s {
    return [self.expression matchesInString:s
                                    options:0
                                      range:NSMakeRange(0, [s length])];
}

- (NSTextCheckingResult*) firstMatch:(NSString*) s {
    return [self.expression firstMatchInString:s
                                       options:0
                                         range:NSMakeRange(0, [s length])];
}

- (BOOL) test:(NSString *)s {
    return [self.expression numberOfMatchesInString:s
                                            options:0
                                              range:NSMakeRange(0, [s length])] > 0;
}

- (NSString *)description {
    return self.pattern;
}

@end
