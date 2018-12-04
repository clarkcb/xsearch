#import <Foundation/Foundation.h>
#import "SearchOption.h"
#import "SearchSettings.h"

@interface SearchOptions : NSObject

- (NSArray<SearchOption*>*) searchOptionsFromJson;
- (SearchSettings*) settingsFromArgs:(NSArray*)args error:(NSError**)error;
- (void) settingsFromData:(NSData *)data settings:(SearchSettings *)settings;
- (NSString*) getUsageString;
- (void) usage:(int)code;

@end
