#import "SearchConfig.h"

@implementation SearchConfig

- (instancetype) init {
    self = [super init];
    if (self) {
        NSString *homePath = [[[NSProcessInfo processInfo] environment] objectForKey:@"HOME"];
        NSString *xSearchConfigDir = [[[NSProcessInfo processInfo] environment] objectForKey:@"XSEARCH_CONFIG_DIR"];
        if (xSearchConfigDir == nil) {
            xSearchConfigDir = [NSString pathWithComponents:@[homePath, @".config", @"xsearch"]];
        }
        NSString *xsearchPath = [[[NSProcessInfo processInfo] environment] objectForKey:@"XSEARCH_PATH"];
        if (xsearchPath == nil) {
            xsearchPath = [NSString pathWithComponents:@[homePath, @"src", @"xsearch"]];
        }
        self.xsearchPath = xsearchPath;
        self.searchOptionsPath = [NSString pathWithComponents:@[xsearchPath, @"shared", @"searchoptions.json"]];
        self.defaultSearchSettingsPath = [NSString pathWithComponents:@[xSearchConfigDir, @"settings.json"]];
    }
    return self;
}

@end
