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
	UInt8 type = KBValueTypeFieldWithInfo(4, YES, KBValueTypeInteger);
	NSNumber * number = [NSNumber numberWithSInt32:myInt];
	return (self = [super initWithValueType:type object:number]);
}

- (id)initWithObject:(NSObject *)object {
	if (![object isKindOfClass:[NSNumber class]]) {
		[super dealloc];
		return nil;
	}
	return ([self initWithInt32:[(NSNumber *)object SInt32Value]]);
}

- (id)initWithValueBytes:(const char *)buffer length:(NSUInteger *)lenInOut {
	if (*lenInOut < 5) {
		[super dealloc];
		return nil;
	}
	valueType = buffer[0];
	if (![self isNullTerminated] || [self lengthFieldLength] != 4) {
		[super dealloc];
		return nil;
	}
	UInt32 number = _ExpandIntAndMakeNativeEndian((const unsigned char *)&buffer[1], 4);
	*lenInOut = 5;
	return (self = [self initWithInt32:(SInt32)number]);
}

- (SInt32)intValue {
	return [(NSNumber *)valueObject SInt32Value];
}

- (BOOL)encodeToData:(NSMutableData *)theData {
	[super encodeToData:theData];
	unsigned char buffer[4];
	_TrimIntAndMakeLittleEndian((UInt32)[self intValue], buffer, 4);
	[theData appendBytes:buffer length:4];
	return YES;
}

@end
