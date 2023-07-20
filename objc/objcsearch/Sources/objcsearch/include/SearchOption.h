#ifndef SearchOption_h
#define SearchOption_h

#import <Foundation/Foundation.h>

@interface SearchOption : NSObject

@property NSString *shortArg;
@property NSString *longArg;
@property NSString *desc;

- (instancetype) initWithShortArg:(NSString*)sArg withLongArg:(NSString*)lArg withDesc:(NSString*)desc;

- (NSString *) sortArg;

@end

#endif /* SearchOption_h */
