//
//  KBValue.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/22/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

typedef enum {
	KBValueTypeTerminator = 0,
	KBValueTypeUTF8String = 1,
	KBValueTypeArray = 2,
	KBValueTypeDictionary = 3,
	KBValueTypeNull = 4,
	KBValueTypeData = 5,
	KBValueTypeInteger = 6,
	KBValueTypeFloating = 7
} KBValueTypeNumber;

// Length field
BOOL _IsBigEndian (void);
void _TrimIntAndMakeLittleEndian (UInt32 anInt, unsigned char * dest, NSInteger trimLength);
UInt32 _ExpandIntAndMakeNativeEndian (unsigned const char * buffer, NSInteger bufferLength);
NSInteger KBValueReadBufferWithInfo (UInt8 valueType, NSMutableData * destination, const char * bytes, NSUInteger length);
// Type field
UInt8 KBValueTypeFieldLenLenForBytes (NSUInteger numBytes);
UInt8 KBValueTypeFieldWithInfo (UInt8 lenLength, BOOL nullterm, KBValueTypeNumber typeNumber);

@interface KBValue : NSObject {
	UInt8 valueType;
	NSObject * valueObject;
}

- (id)initWithValueType:(UInt8)type object:(NSObject *)object;
// Abstract encode
- (id)initWithObject:(NSObject *)object;
// Decode
- (id)initWithValueBytes:(const char *)buffer length:(NSUInteger *)lenInOut;
- (id)initWithValueData:(NSData *)data;

- (UInt8)valueType;
- (NSObject *)valueObject;

// type components
- (KBValueTypeNumber)valueTypeNumber;
- (UInt8)lengthFieldLength;
- (BOOL)isNullTerminated;

- (NSData *)encodeValue;
- (BOOL)encodeToData:(NSMutableData *)theData;

@end
