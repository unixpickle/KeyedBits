//
//  KBValue.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/22/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBValue.h"

@implementation KBValue

- (id)initWithObject:(NSObject *)anObject {
	[[super init] dealloc];
	return nil;
}

- (id)initWithValueType:(UInt8)type object:(NSObject *)object {
	if ((self = [super init])) {
		valueType = type;
		valueObject = [object retain];
	}
	return self;
}

- (id)initWithValueBytes:(const char *)buffer length:(NSUInteger *)lenInOut {
	[[super init] dealloc];
	return nil;
}

- (id)initWithValueData:(NSData *)data {
	NSUInteger length = [data length];
	const char * bytes = (const char *)[data bytes];
	return (self = [self initWithValueBytes:bytes length:&length]);
}

// Getters

- (UInt8)valueType {
	return valueType;
}

- (NSObject *)valueObject {
	return valueObject;
}

// Type Components

- (KBValueTypeNumber)valueTypeNumber {
	UInt8 inf = 7 & valueType;
	return (KBValueTypeNumber)inf;
}

- (UInt8)lengthFieldLength {
	return ((valueType >> 5) & 3) + 1;
}

- (BOOL)isNullTerminated {
	if ((valueType & 128) != 0) {
		return YES;
	}
	return NO;
}

// Encoding

- (NSData *)encodeValue {
	NSMutableData * encoded = [[NSMutableData alloc] init];
	if (![self encodeToData:encoded]) {
		[encoded release];
		return nil;
	}
	NSData * immutable = [NSData dataWithData:encoded];
	[encoded release];
	return immutable;
}

- (BOOL)encodeToData:(NSMutableData *)theData {
	[theData appendBytes:&valueType length:1];
	return NO;
}

- (void)dealloc {
	[valueObject release];
	[super dealloc];
}

@end

// C function implementations

BOOL _IsBigEndian (void) {
	UInt16 myInt = 1;
	char * bytes = (char *)&myInt;
	if (bytes[0] == 1) return NO;
	else return YES;
}

void _TrimIntAndMakeLittleEndian (UInt32 anInt, unsigned char * dest, NSInteger trimLength) {
	unsigned char * intBuffer = (unsigned char *)&anInt;
	BOOL bigEndian = _IsBigEndian();
	UInt8 offset = (bigEndian ? (4 - trimLength) : 0);
	for (int i = 0; i < trimLength; i++) {
		UInt8 byteIndex = (bigEndian ? (trimLength - (i + 1)) : i);
		dest[i] = intBuffer[offset + byteIndex];
	}
}

UInt32 _ExpandIntAndMakeNativeEndian (unsigned const char * buffer, NSInteger bufferLength) {
	UInt32 answer = 0;
	unsigned char * intBuffer = (unsigned char *)&answer;
	BOOL bigEndian = _IsBigEndian();
	NSInteger startIndex = bigEndian ? ((NSInteger)4 - bufferLength) : 0;
	
	for (int i = 0; i < bufferLength; i++) {
		UInt8 charIndex = bigEndian ? (bufferLength - (i + 1)) : i;
		intBuffer[charIndex + startIndex] = buffer[i];
	}
	
	return answer;
}

NSInteger KBValueReadBufferWithInfo (UInt8 valueType, NSMutableData * destination, const char * bytes, NSUInteger length) {
	BOOL isNullTerm = (valueType >> 7) & 1;
	UInt8 lenLength = ((valueType >> 5) & 3) + 1;
	if (isNullTerm) {
		NSInteger i;
		for (i = 0; i < length; i++) {
			char byte = bytes[i];
			if (byte == 0) break;
			[destination appendBytes:&bytes[i] length:1];
			if (i + 1 == length) return -1; // no NULL terminator
		}
		return i + 1;
	} else {
		// read length field, then read buffer
		if (lenLength > length) {
			return -1;
		}
		UInt32 contLen = _ExpandIntAndMakeNativeEndian((const unsigned char *)bytes, lenLength);
		if (lenLength + contLen > length) {
			return -1;
		}
		[destination appendBytes:&bytes[lenLength] length:contLen];
		return lenLength + contLen;
	}
}

UInt8 KBValueTypeFieldLenLenForBytes (NSUInteger numBytes) {
	if (numBytes <= 0xff) {
		return 1;
	} else if (numBytes <= 0xffff) {
		return 2;
	} else if (numBytes <= 0xffffff) {
		return 3;
	} else if (numBytes <= 0xffffffff) {
		return 4;
	} else {
		return 0;
	}
}

UInt8 KBValueTypeFieldWithInfo (UInt8 lenLength, BOOL nullterm, KBValueTypeNumber typeNumber) {
	UInt8 result = 0;
	if (lenLength > 4 || typeNumber > 7) {
		return 0;
	}
	
	if (nullterm) {
		result |= 128; // set NULL term
	}
	result |= (lenLength - 1) << 5; // set lenLen
	
	result |= typeNumber;
	return result;
}
