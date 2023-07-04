#import "common.h"

void logMsg(NSString *s) {
    //NSLog(@"%@", s);
    printf("%s\n", [s UTF8String]);
}

void logError(NSString *s) {
    //NSLog(@"ERROR: %@", s);
    printf("ERROR: %s\n", [s UTF8String]);
}

void setError(NSError **error, NSString *msg) {
    NSString *domain = msg;
    NSDictionary *userInfo = [[NSDictionary alloc] init];
    *error = [NSError errorWithDomain:domain code:500 userInfo:userInfo];
}

NSString* boolToNSString(BOOL b) {
    switch (b) {
        case 1:
            return @"true";
        default:
            return @"false";
    }
}

NSString* arrayToNSString(NSArray *arr) {
    NSMutableString *arrString = [NSMutableString stringWithString:@"["];
    for (int i=0; i < [arr count]; i++) {
        if (i > 0) {
            [arrString appendString:@", "];
        }
        if ([arr[i] isKindOfClass:[NSString class]]) {
            [arrString appendFormat:@"\"%@\"", arr[i]];
        } else {
            [arrString appendFormat:@"%@", arr[i]];
        }
    }
    [arrString appendString:@"]"];
    return [NSString stringWithString:arrString];
}
