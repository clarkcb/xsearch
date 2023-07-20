#import "SearchConfig.h"

NSString* getXsearchPath() {
    NSString *xsearchPath = [[[NSProcessInfo processInfo] environment] objectForKey:@"XSEARCH_PATH"];
    if (xsearchPath == nil) {
        xsearchPath = [[[[NSProcessInfo processInfo] environment] objectForKey:@"HOME"] stringByAppendingString:@"/src/xsearch"];
    }
    return xsearchPath;
}

NSString* getXsearchSharedPath() {
    NSString *xsearchPath = getXsearchPath();
    return [xsearchPath stringByAppendingPathComponent:@"shared"];
}
