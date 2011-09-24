//
//  KBValueInteger.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/23/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBValueInteger.h"

@implementation KBValueInteger

- (id)initWithInt32:(SInt32)myInt {
	UInt8 type = KBValueTypeFieldWithInfo(2, YES, KBValueTypeInteger);
	NSNumber * number = [NSNumber numberWithSInt32:myInt];
	return (self = [super initWithValueType:type object:number]);
}

- (id)initWithInt64:(SInt64)bigInt {
	UInt8 type = KBValueTypeFieldWithInfo(3, YES, KBValueTypeInteger);
	NSNumber * number = [NSNumber numberWithSInt64:bigInt];
	return (self = [super initWithValueType:type object:number]);
}

- (id)initWithObject:(NSObject *)object {
	if (![object isKindOfClass:[NSNumber class]]) {
		[super dealloc];
		return nil;
	}
	NSNumber * number = (NSNumber *)object;
	if ([number intNeeds64Bytes]) {
		return ([self initWithInt64:[number SInt64Value]]);
	} else {
		return ([self initWithInt32:[number SInt32Value]]);
	}
}

- (id)initWithValueBytes:(const char *)buffer length:(NSUInteger *)lenInOut {
	if (*lenInOut < 1) {
		[super dealloc];
		return nil;
	}
	valueType = buffer[0];
	if (![self isNullTerminated]) {
		[super dealloc];
		return nil;
	}
	UInt8 intLen = 1 << ([self lengthFieldLength]);
	if (intLen + 1 > *lenInOut) {
		[super dealloc];
		return nil;
	}
	if (intLen == 4) {
		UInt32 number = (UInt32)_ExpandIntAndMakeNativeEndian((const unsigned char *)&buffer[1], intLen);
		*lenInOut = 5;
		return (self = [self initWithInt32:(SInt32)number]);
	} else if (intLen == 8) {
		UInt64 number = _ExpandIntAndMakeNativeEndian((const unsigned char *)&buffer[1], intLen);
		*lenInOut = 9;
		return (self = [self initWithInt64:(SInt64)number]);
	} else {
		[super dealloc];
		return nil;
	}
}

- (SInt32)intValue {
	return [(NSNumber *)valueObject SInt32Value];
}

- (SInt64)int64Value {
	return [(NSNumber *)valueObject SInt64Value];
}

- (BOOL)encodeToData:(NSMutableData *)theData {
	UInt8 intLen = 1 << ([self lengthFieldLength]);
	[super encodeToData:theData];
	unsigned char buffer[8];
	if (intLen == 4) {
		_TrimIntAndMakeLittleEndian((UInt32)[self intValue], buffer, intLen);
	} else if (intLen == 8) {
		_TrimIntAndMakeLittleEndian((UInt64)[self int64Value], buffer, intLen);
	} else {
		return NO;
	}
	[theData appendBytes:buffer length:intLen];
	return YES;
}

@end
