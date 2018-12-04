#import <Foundation/Foundation.h>
#import "common.h"

@interface FileTypes : NSObject

- (NSDictionary*) fileTypesFromJson;
+ (FileType) fromName:(NSString*)typeName;
+ (NSString*) toName:(FileType)fileType;
- (FileType) getFileType:(NSString*)fileName;
- (BOOL) isArchiveFile:(NSString*)fileName;
- (BOOL) isBinaryFile:(NSString*)fileName;
- (BOOL) isCodeFile:(NSString*)fileName;
- (BOOL) isTextFile:(NSString*)fileName;
- (BOOL) isXmlFile:(NSString*)fileName;

@end
