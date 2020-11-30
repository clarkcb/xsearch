#import <Foundation/Foundation.h>
#import "color.h"
#import "SearchResult.h"
#import "SearchSettings.h"

@interface SearchResultFormatter : NSObject

@property SearchSettings *settings;

- (instancetype) initWithSettings:(SearchSettings*)settings;

- (NSString *) format:(SearchResult*)result;

@end
