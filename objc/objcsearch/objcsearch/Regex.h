#import <Foundation/Foundation.h>

@interface Regex : NSObject

@property NSRegularExpression *expression;
@property NSString *pattern;

- (instancetype) initWithPattern:(NSString*) pattern;
- (NSArray *) matches:(NSString*) s;
- (BOOL) test:(NSString*) s;

@end
