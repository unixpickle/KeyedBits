//
//  KBObjectCoder.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/22/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBObjectCoder.h"

@implementation KBObjectCoder

+ (Class)valueClassForType:(UInt8)type {
	switch ((type & 7)) {
		case KBValueTypeUTF8String:
			return [KBValueString class];
		case KBValueTypeData:
			return [KBValueData class];
		case KBValueTypeArray:
			return [KBValueArray class];
		case KBValueTypeInteger:
			return [KBValueInteger class];
		default:
			break;
	}
	return Nil;
}

+ (Class)valueClassForKeyedData:(NSData *)data {
	if ([data length] == 0) {
		return Nil;
	}
	const char * bytes = (const char *)[data bytes];
	return [self valueClassForType:bytes[0]];
}

+ (Class)valueClassForObject:(NSObject *)anObject {
	if ([anObject isKindOfClass:[NSString class]]) {
		return [KBValueString class];
	} else if ([anObject isKindOfClass:[NSData class]]) {
		return [KBValueData class];
	} else if ([anObject isKindOfClass:[NSArray class]]) {
		return [KBValueArray class];
	} else if ([anObject isKindOfClass:[NSNumber class]]) {
		// there are two datatypes for numbers
		if ([(NSNumber *)anObject isInteger]) {
			return [KBValueInteger class];
		}
	}
	return Nil;
}

@end
