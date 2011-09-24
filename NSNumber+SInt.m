//
//  NSNumber+SInt.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/23/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "NSNumber+SInt.h"

@implementation NSNumber (SInt)

+ (NSNumber *)numberWithSInt32:(SInt32)signedInt32 {
#if INT_MAX == INT32_MAX
	return [NSNumber numberWithInt:signedInt32];
#elif LONG_MAX == INT32_MAX
	return [NSNumber numberWithLong:signedInt32];
#elif SHORT_MAX == INT32_MAX
	return [NSNumber numberWithShort:signedInt32];
#endif
}

+ (NSNumber *)numberWithSInt64:(SInt64)signedInt64 {
#if INT_MAX == INT64_MAX
	return [NSNumber numberWithInt:signedInt64];
#elif LONG_MAX == INT64_MAX
	return [NSNumber numberWithLong:signedInt64];
#elif SHORT_MAX == INT64_MAX
	return [NSNumber numberWithShort:signedInt64];
#elif LONG_LONG_MAX == INT64_MAX
	return [NSNumber numberWithLongLong:signedInt64];
#endif
}

- (SInt32)SInt32Value {
#if INT_MAX == INT32_MAX
	return [self intValue];
#elif LONG_MAX == INT32_MAX
	return [self longValue];
#elif SHORT_MAX == INT32_MAX
	return [self shortValue];
#endif
}

- (SInt64)SInt64Value {
#if INT_MAX == INT64_MAX
	return [self intValue];
#elif LONG_MAX == INT64_MAX
	return [self longValue];
#elif SHORT_MAX == INT64_MAX
	return [self shortValue];
#elif LONG_LONG_MAX == INT64_MAX
	return [self longLongValue];
#endif
}

- (BOOL)intNeeds64Bytes {
	if ([self SInt64Value] != [self SInt32Value]) {
		return YES;
	}
	return NO;
}

- (BOOL)isInteger {
	const char * type = [self objCType];
	if (strcmp(type, @encode(short)) == 0) {
		return YES;
	} else if (strcmp(type, @encode(int)) == 0) {
		return YES;
	} else if (strcmp(type, @encode(long)) == 0) {
		return YES;
	} else if (strcmp(type, @encode(long long)) == 0) {
		return YES;
	} else if (strcmp(type, @encode(unsigned short)) == 0) {
		return YES;
	} else if (strcmp(type, @encode(unsigned long)) == 0) {
		return YES;
	} else if (strcmp(type, @encode(unsigned int)) == 0) {
		return YES;
	} else if (strcmp(type, @encode(unsigned long long)) == 0) {
		return YES;
	} else if (strcmp(type, @encode(BOOL)) == 0) {
		return YES;
	} else if (strcmp(type, @encode(unsigned char)) == 0) {
		return YES;
	} else if (strcmp(type, @encode(char)) == 0) {
		return YES;
	}
	return NO;
}

@end
