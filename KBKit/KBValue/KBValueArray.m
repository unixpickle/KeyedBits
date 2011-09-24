//
//  KBValueArray.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/23/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBValueArray.h"
#import "NSObject+KeyedBits.h"

@implementation KBValueArray

- (id)initWithArray:(NSArray *)anArray {
	return (self = [self initWithArray:anArray]);
}

- (id)initWithObject:(NSObject *)object {
	if (![object isKindOfClass:[NSArray class]]) {
		[super dealloc];
		return nil;
	}
	UInt8 type = KBValueTypeFieldWithInfo(1, YES, KBValueTypeArray);
	return (self = [super initWithValueType:type object:object]);
}

- (id)initWithValueBytes:(const char *)buffer length:(NSUInteger *)lenInOut {
	if ((self = [super init])) {
		if (*lenInOut < 2) {
			[super dealloc];
			return nil;
		}
		valueType = buffer[0];
		if (![self isNullTerminated] || [self lengthFieldLength] != 1) {
			[super dealloc];
			return nil;
		}
		
		BOOL correctlyTerminated = NO;
		NSMutableArray * decoded = [[NSMutableArray alloc] init];
		NSUInteger bytesLeft = 0;
		for (NSInteger index = 1; index < *lenInOut; index += bytesLeft) {
			Class decodeClass;
			KBValue * value;
			// Number of bytes that can be used for the object.
			// Note that we subtract an extra byte because we
			// know that this array is NULL terminated.
			bytesLeft = *lenInOut - (index + 1);
			if ((buffer[index] & 7) == 0) {
				correctlyTerminated = YES;
				*lenInOut = index + 1;
				break;
			}
			decodeClass = [KBObjectCoder valueClassForType:buffer[index]];
			if (decodeClass == Nil) {
				break;
			}
			// decode with the current bytes
			value = [(KBValue *)[decodeClass alloc] initWithValueBytes:&buffer[index] length:&bytesLeft];
			if (!value) {
				break;
			}
			[decoded addObject:[value valueObject]];
			[value release];
		}
		
		if (!correctlyTerminated) {
			[decoded release];
			[super dealloc];
			return nil;
		}
		valueObject = decoded;
	}
	return self;
}

- (NSArray *)array {
	return (NSArray *)[self valueObject];
}

- (BOOL)encodeToData:(NSMutableData *)theData {
	[super encodeToData:theData];
	char terminator = 0;
	NSArray * array = [self array];
	for (int i = 0; i < [array count]; i++) {
		NSObject * object = [array objectAtIndex:i];
		KBValue * encoded = [object keyedBitsValue];
		if (!encoded) {
			return NO;
		}
		if (![encoded encodeToData:theData]) {
			return NO;
		}
	}
	[theData appendBytes:&terminator length:1];
	return YES;
}

@end
