//
//  KBValueData.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/22/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBValueData.h"

@implementation KBValueData

- (id)initWithContentData:(NSData *)data {
	return (self = [self initWithObject:data]);
}

- (id)initWithObject:(NSObject *)object {
	if (![object isKindOfClass:[NSData class]]) {
		[[super init] dealloc];
		return nil;
	}
	UInt8 lenLen = KBValueTypeFieldLenLenForBytes([(NSData *)object length]);
	if (lenLen == 0) {
		[[super init] dealloc];
		return nil;
	}
	UInt8 typeInfo = KBValueTypeFieldWithInfo(lenLen, NO, KBValueTypeData);
	return (self = [super initWithValueType:typeInfo object:object]);
}

- (id)initWithValueBytes:(const char *)buffer length:(NSUInteger *)lenInOut {
	if (self = [super init]) {
		if (*lenInOut == 0) {
			[super dealloc];
			return nil;
		}
		valueType = buffer[0];
		
		NSMutableData * rawData = [[NSMutableData alloc] init];
		NSInteger readLength = KBValueReadBufferWithInfo(valueType, rawData, &buffer[1], *lenInOut - 1);
		if (readLength < 0) {
			[rawData release];
			[super dealloc];
			return nil;
		}
		*lenInOut = readLength + 1;
		
		valueObject = rawData;
	}
	return self;
}

- (NSData *)contentData {
	return (NSData *)valueObject;
}

- (BOOL)encodeToData:(NSMutableData *)theData {
	[super encodeToData:theData];
	char numbuffer[4];
	_TrimIntAndMakeLittleEndian((UInt32)[[self contentData] length], (unsigned char *)numbuffer, [self lengthFieldLength]);
	[theData appendBytes:numbuffer length:[self lengthFieldLength]];
	[theData appendData:[self contentData]];
	return YES;
}

@end
