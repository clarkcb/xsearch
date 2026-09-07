#ifndef SearchConfig_h
#define SearchConfig_h

#import <Foundation/Foundation.h>
#import "FindConfig.h"

@interface SearchConfig : FindConfig

@property NSString *xsearchPath;
@property NSString *searchOptionsPath;
@property NSString *defaultSearchSettingsPath;

- (instancetype) init;

@end

#endif /* SearchConfig_h */
