#ifndef SearchOptions_h
#define SearchOptions_h

#import <Foundation/Foundation.h>
#import "SearchOption.h"
#import "SearchSettings.h"

@interface SearchOptions : NSObject

- (NSArray<SearchOption*>*) searchOptionsFromJson;
- (SearchSettings*) settingsFromArgs:(NSArray*)args error:(NSError**)error;
- (void) updateSettingsFromData:(NSData *)data settings:(SearchSettings *)settings error:(NSError**)error;;
- (SearchSettings*) settingsFromData:(NSData *)data error:(NSError **)error;
- (NSString*) getUsageString;
- (void) usage:(int)code;

@end

#endif /* SearchOptions_h */
