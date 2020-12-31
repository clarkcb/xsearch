#import "config.h"
#import "FileTypes.h"
#import "FileUtil.h"

@interface FileTypes ()


@property NSString *archive;
@property NSString *binary;
@property NSString *code;
@property NSString *text;
@property NSString *unknown;
@property NSString *xml;


@property NSDictionary<NSString*,NSSet<NSString*>*> *fileTypesDict;

@end

@implementation FileTypes

- (instancetype) init {
    self = [super init];
    if (self) {
        self.archive = [NSString stringWithUTF8String:T_ARCHIVE];
        self.binary = [NSString stringWithUTF8String:T_BINARY];
        self.code = [NSString stringWithUTF8String:T_CODE];
        self.text = [NSString stringWithUTF8String:T_TEXT];
        self.unknown = [NSString stringWithUTF8String:T_UNKNOWN];
        self.xml = [NSString stringWithUTF8String:T_XML];
        self.fileTypesDict = [self fileTypesFromJson];
    }
    return self;
}

- (NSDictionary<NSString*,NSSet<NSString*>*>*) fileTypesFromJson {
    NSMutableString *fileTypesJsonPath = [NSMutableString stringWithUTF8String:SHAREDPATH];
    [fileTypesJsonPath appendString:@"/filetypes.json"];

    NSMutableDictionary *fileTypesDict = [[NSMutableDictionary alloc] init];
    
    if (![[NSFileManager defaultManager] fileExistsAtPath:fileTypesJsonPath]) {
        return nil;
    }

    NSData *data = [NSData dataWithContentsOfFile:fileTypesJsonPath];

    if (NSClassFromString(@"NSJSONSerialization")) {
        NSError *error = nil;
        id jsonObject = [NSJSONSerialization
                         JSONObjectWithData:data
                         options:0
                         error:&error];

        if (error) { /* JSON was malformed, act appropriately here */ }

        if ([jsonObject isKindOfClass:[NSDictionary class]]) {
            NSArray *fileTypes = jsonObject[@"filetypes"];
            for (NSDictionary *typeDict in fileTypes) {
                NSArray *extensions = typeDict[@"extensions"];
                NSString *type = typeDict[@"type"];
                fileTypesDict[type] = [NSSet setWithArray:extensions];
            }
        }
    }
    return [NSDictionary dictionaryWithDictionary:fileTypesDict];
}

+ (FileType) fromName:(NSString*)typeName {
    NSString *lname = [typeName lowercaseString];
    if (lname == [NSString stringWithUTF8String:T_TEXT]) {
        return FileTypeText;
    }
    if (lname == [NSString stringWithUTF8String:T_BINARY]) {
        return FileTypeBinary;
    }
    if (lname == [NSString stringWithUTF8String:T_ARCHIVE]) {
        return FileTypeArchive;
    }
    if (lname == [NSString stringWithUTF8String:T_CODE]) {
        return FileTypeCode;
    }
    if (lname == [NSString stringWithUTF8String:T_XML]) {
        return FileTypeXml;
    }
    return FileTypeUnknown;
}

+ (NSString*) toName:(FileType)fileType {
    if (fileType == FileTypeText) {
        return [NSString stringWithUTF8String:T_TEXT];
    }
    if (fileType == FileTypeBinary) {
        return [NSString stringWithUTF8String:T_BINARY];
    }
    if (fileType == FileTypeArchive) {
        return [NSString stringWithUTF8String:T_ARCHIVE];
    }
    if (fileType == FileTypeCode) {
        return [NSString stringWithUTF8String:T_CODE];
    }
    if (fileType == FileTypeXml) {
        return [NSString stringWithUTF8String:T_XML];
    }
    return [NSString stringWithUTF8String:T_UNKNOWN];
}

- (FileType) getFileType:(NSString*)fileName {
    if ([self isCodeFile:fileName]) {
        return FileTypeCode;
    }
    if ([self isXmlFile:fileName]) {
        return FileTypeXml;
    }
    if ([self isTextFile:fileName]) {
        return FileTypeText;
    }
    if ([self isBinaryFile:fileName]) {
        return FileTypeBinary;
    }
    if ([self isArchiveFile:fileName]) {
        return FileTypeArchive;
    }
    return FileTypeUnknown;
}

- (BOOL) isFileOfType:(NSString*)fileName type:(NSString*)typeName {
    NSString *ext = [FileUtil getExtension:fileName];
    return self.fileTypesDict[typeName] != nil &&
    [self.fileTypesDict[typeName] containsObject:ext];
}

- (BOOL) isArchiveFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.archive];
}

- (BOOL) isBinaryFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.binary];
}

- (BOOL) isCodeFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.code];
}

- (BOOL) isTextFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.text];
}

- (BOOL) isXmlFile:(NSString*)fileName {
    return [self isFileOfType:fileName type:self.xml];
}

- (BOOL) isSearchableFile:(NSString*)fileName {
    return [self getFileType:fileName] != FileTypeUnknown;
}

- (BOOL) isUnknownFile:(NSString*)fileName {
    return [self getFileType:fileName] == FileTypeUnknown;
}

@end
