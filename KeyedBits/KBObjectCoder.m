//
//  KBObjectCoder.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/22/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBObjectCoder.h"

@implementation KBObjectCoder

+ (Class)valueClassForKeyedData:(NSData *)data {
	if ([data length] == 0) {
		return Nil;
	}
	const char * bytes = (const char *)[data bytes];
	switch ((bytes[0] & 7)) {
		case KBValueTypeUTF8String:
			return [KBValueString class];
			break;
		default:
			break;
	}
	return Nil;
}

+ (Class)valueClassForObject:(NSObject *)anObject {
	if ([anObject isKindOfClass:[NSString class]]) {
		return [KBValueString class];
	}
	return Nil;
}

@end
