#ifndef common_h
#define common_h

#import <Foundation/Foundation.h>

// file type names
#define T_ARCHIVE "archive"
#define T_BINARY "binary"
#define T_CODE "code"
#define T_TEXT "text"
#define T_UNKNOWN "unknown"
#define T_XML "xml"

// file type enum
typedef enum {
    FileTypeUnknown = -1,
    FileTypeArchive = 0,
    FileTypeBinary,
    FileTypeCode,
    FileTypeText,
    FileTypeXml
} FileType;

// common functions
void logMsg(NSString *s);
void logError(NSString *s);
void setError(NSError **e, NSString *msg);
NSString * boolToNSString(BOOL b);
NSString * arrayToNSString(NSArray *arr);

#endif /* common_h */
