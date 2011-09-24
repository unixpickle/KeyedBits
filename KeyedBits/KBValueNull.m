//
//  KBValueNull.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/23/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBValueNull.h"

@implementation KBValueNull

- (id)initWithObject:(NSObject *)object {
	if (![object isKindOfClass:[NSNull class]]) {
		[super dealloc];
		return nil;
	}
	if ((self = [super initWithValueType:KBValueTypeNull object:object])) {
	}
	return self;
}

- (id)init {
	return (self = [self initWithObject:[NSNull null]]);
}

- (id)initWithValueBytes:(const char *)buffer length:(NSUInteger *)lenInOut {
	if (*lenInOut == 0) {
		[super dealloc];
		return nil;
	}
	*lenInOut = 1;
	return (self = [self init]);
}

- (BOOL)encodeToData:(NSMutableData *)theData {
	[super encodeToData:theData];
	return YES;
}

@end
