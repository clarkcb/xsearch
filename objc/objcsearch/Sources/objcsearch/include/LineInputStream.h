#import <Foundation/Foundation.h>

@interface LineInputStream : NSInputStream

/*
 read:maxLength:

 Reads up to a given number of bytes into a given buffer.
 
 Parameters
 buffer - A data buffer. The buffer must be large enough to contain the number of bytes specified by len.
 len - The maximum number of bytes to read.
 
 Return Value
 A number indicating the outcome of the operation:
 A positive number indicates the number of bytes read.
 0 indicates that the end of the buffer was reached.
 -1 means that the operation failed; more information about the error can be obtained with streamError.
 */
- (NSInteger) read:(uint8_t *)buffer maxLength:(NSUInteger)len;

/*
 getBuffer:length:

 Returns by reference a pointer to a read buffer and, by reference, the number of bytes available, and returns a Boolean value that indicates whether the buffer is available.
 
 Parameters
 buffer
 A data buffer. The buffer must be large enough to contain the number of bytes specified by len.
 
 len
 The maximum number of bytes to read.
 
 Return Value
 A number indicating the outcome of the operation:
 
 A positive number indicates the number of bytes read.
 0 indicates that the end of the buffer was reached.
 -1 means that the operation failed; more information about the error can be obtained with streamError.
 */
- (BOOL)getBuffer:(uint8_t * _Nullable *)buffer length:(NSUInteger *)len;

/*
 hasBytesAvailable
 
 A Boolean value that indicates whether the receiver has bytes available to read.
 */
@property(readonly) BOOL hasBytesAvailable;

@end
