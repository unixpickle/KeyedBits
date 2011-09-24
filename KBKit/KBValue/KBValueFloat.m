//
//  KBValueFloat.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/23/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBValueFloat.h"

@implementation KBValueFloat

- (id)initWithDouble:(double)d {
	NSNumber * object = [NSNumber numberWithDouble:d];
	return (self = [self initWithObject:object]);
}

- (id)initWithFloat:(float)f {
	NSNumber * object = [NSNumber numberWithFloat:f];
	return (self = [self initWithObject:object]);
}

- (id)initWithObject:(NSObject *)object {
	if (![object isKindOfClass:[NSNumber class]]) {
		[super dealloc];
		return nil;
	}
	UInt8 type = KBValueTypeFieldWithInfo(1, YES, KBValueTypeFloating);
	return (self = [super initWithValueType:type object:object]);
}

- (id)initWithValueBytes:(const char *)buffer length:(NSUInteger *)lenInOut {
	if ((self = [super init])) {
		valueType = buffer[0];
		NSMutableData * stringData = [[NSMutableData alloc] init];
		NSInteger used = KBValueReadBufferWithInfo(valueType, stringData, &buffer[1], *lenInOut - 1);
		if (used < 0) {
			[stringData release];
			[super dealloc];
			return nil;
		}
		
		NSString * string = [[NSString alloc] initWithData:stringData encoding:NSUTF8StringEncoding];
		[stringData release];
		if (!string) {
			[super dealloc];
			return nil;
		}
		valueObject = [[NSNumber alloc] initWithDouble:[string doubleValue]];
		[string release];
	}
	return self;
}

- (double)doubleValue {
	return [(NSNumber *)valueObject doubleValue];
}

- (float)floatValue {
	return [(NSNumber *)valueObject floatValue];
}

- (BOOL)encodeToData:(NSMutableData *)theData {
	[super encodeToData:theData];
	const char * doubleString = [[(NSNumber *)valueObject stringValue] UTF8String];
	[theData appendBytes:doubleString length:(strlen(doubleString) + 1)];
	return YES;
}

@end
