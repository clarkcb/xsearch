#ifndef SearchOption_h
#define SearchOption_h

#import <Foundation/Foundation.h>

#import "Option.h"

@interface SearchOption : NSObject <Option>

@property NSString *shortArg;
@property NSString *longArg;
@property NSString *desc;
@property ArgTokenType argType;

- (instancetype) initWithShortArg:(NSString*)sArg withLongArg:(NSString*)lArg withDesc:(NSString*)desc withArgType:(ArgTokenType)argType;

- (NSString *) sortArg;

@end

#endif /* SearchOption_h */
