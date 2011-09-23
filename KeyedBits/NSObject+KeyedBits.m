//
//  NSObject+KeyedBits.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/22/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "NSObject+KeyedBits.h"

@implementation NSObject (KeyedBits)

+ (id)objectWithKeyedBitsData:(NSData *)theData {
	KBValue * value = nil;
	id anObject;
	Class class = [KBObjectCoder valueClassForKeyedData:theData];
	if (class == Nil) {
		return nil;
	}
	
	value = [(KBValue *)[class alloc] initWithValueData:theData];
	anObject = [[value valueObject] retain];
	[value release];
	
	return [anObject autorelease];
}

- (KBValue *)keyedBitsValue {
	Class class = [KBObjectCoder valueClassForObject:self];
	if (class == Nil) {
		return nil;
	}
	return [[(KBValue *)[class alloc] initWithObject:self] autorelease];
}

@end
