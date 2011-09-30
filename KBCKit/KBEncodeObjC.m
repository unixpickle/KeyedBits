//
//  KBEncodeObjC.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/24/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBEncodeObjC.h"

static bool _kb_encode_objc_number_is_integer (NSNumber * number);
static int32_t _kb_encode_objc_number_sint32 (NSNumber * number);
static int64_t _kb_encode_objc_number_sint64 (NSNumber * number);

void kb_encode_objc_object (KBContextRef ctx, NSObject * anObject) {
	if ([anObject isKindOfClass:[NSString class]]) {
		kb_encode_objc_string(ctx, (NSString *)anObject);
	} else if ([anObject isKindOfClass:[NSArray class]]) {
		kb_encode_objc_array(ctx, (NSArray *)anObject);
	} else if ([anObject isKindOfClass:[NSDictionary class]]) {
		kb_encode_objc_dictionary(ctx, (NSDictionary *)anObject);
	} else if ([anObject isKindOfClass:[NSNull class]]) {
		kb_encode_objc_null(ctx, (NSNull *)anObject);
	} else if ([anObject isKindOfClass:[NSData class]]) {
		kb_encode_objc_data(ctx, (NSData *)anObject);
	} else if ([anObject isKindOfClass:[NSNumber class]]) {
		kb_encode_objc_number(ctx, (NSNumber *)anObject);
	} else {
		@throw [NSException exceptionWithName:NSInvalidArgumentException
									   reason:@"kb_encode_objc_object(): Invalid class of object"
									 userInfo:nil];
	}
}

void kb_encode_objc_dictionary (KBContextRef ctx, NSDictionary * dictionary) {
	kb_encode_begin_dictionary(ctx);
	NSArray * allKeys = [dictionary allKeys];
	int i;
	for (i = 0; i < [allKeys count]; i++) {
		NSString * theKey = [allKeys objectAtIndex:i];
		if (![theKey isKindOfClass:[NSString class]]) {
			@throw [NSException exceptionWithName:NSInvalidArgumentException
										   reason:@"kb_encode_objc_dictionary(): All keys must be instances of NSString"
										 userInfo:nil];
		}
		kb_encode_dictionary_key(ctx, [theKey UTF8String]);
		kb_encode_objc_object(ctx, [dictionary objectForKey:theKey]);
	}
	kb_encode_end_dictionary(ctx);
}

void kb_encode_objc_array (KBContextRef ctx, NSArray * array) {
	kb_encode_begin_array(ctx);
	int i;
	for (i = 0; i < [array count]; i++) {
		NSObject * object = [array objectAtIndex:i];
		kb_encode_objc_object(ctx, object);
	}
	kb_encode_end_array(ctx);
}

void kb_encode_objc_number (KBContextRef ctx, NSNumber * number) {
	if (_kb_encode_objc_number_is_integer(number)) {
		int32_t int32 = _kb_encode_objc_number_sint32(number);
		int64_t int64 = _kb_encode_objc_number_sint64(number);
		if (int32 != int64) {
			kb_encode_integer64(ctx, int64);
		} else {
			kb_encode_integer32(ctx, int32);
		}
	} else {
		kb_encode_double(ctx, [number doubleValue]);
	}
}

void kb_encode_objc_string (KBContextRef ctx, NSString * string) {
	const char * utf8String = [string UTF8String];
	kb_encode_string(ctx, utf8String, strlen(utf8String));
}

void kb_encode_objc_data (KBContextRef ctx, NSData * data) {
	kb_encode_data(ctx, [data bytes], [data length]);
}

void kb_encode_objc_null (KBContextRef ctx, NSNull * null) {
	kb_encode_null(ctx);
}

NSData * kb_encode_full (NSObject * anObject) {
	KBContextRef ctx = kb_context_create();
	kb_encode_objc_object(ctx, anObject);
	NSData * data = [NSData dataWithBytesNoCopy:ctx->buffer length:ctx->usedLength freeWhenDone:YES];
	free(ctx);
	return data;
}

BOOL kb_encode_full_fd (NSObject * anObject, int filedesc) {
	KBContextRef ctx = kb_context_create_file(filedesc);
	kb_encode_objc_object(ctx, anObject);
	if (ctx->fdesc < 0) {
		kb_context_free(ctx);
		return NO;
	}
	kb_context_free(ctx);
	return YES;
}

// Numbers

static bool _kb_encode_objc_number_is_integer (NSNumber * number) {
	const char * type = [number objCType];
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

static int32_t _kb_encode_objc_number_sint32 (NSNumber * number) {
#if __WORDSIZE == 64
	return [number intValue];
#elif __WORDSIZE == 32
	return [number intValue];
#else
	if (sizeof(int) == sizeof(int32_t)) {
		return (int32_t)[number intValue];
	} else if (sizeof(long) == sizeof(int32_t)) {
		return (int32_t)[number longValue];
	} else if (sizeof(short) == sizeof(int32_t)) {
		return (int32_t)[number shortValue];
	} else if (sizeof(long long) == sizeof(int32_t)) {
		return (int32_t)[number longLongValue];
	} else {
		NSLog(@"Warning, no suitable type found.");
		return 0;
	}
#endif
}

static int64_t _kb_encode_objc_number_sint64 (NSNumber * number) {
#if __WORDSIZE == 64
	return [number longValue];
#elif __WORDSIZE == 32
	return [number longLongValue];
#else
	if (sizeof(int) == sizeof(int64_t)) {
		return (int64_t)[number intValue];
	} else if (sizeof(long) == sizeof(int64_t)) {
		return (int64_t)[number longValue];
	} else if (sizeof(short) == sizeof(int64_t)) {
		return (int64_t)[number shortValue];
	} else if (sizeof(long long) == sizeof(int64_t)) {
		return (int64_t)[number longLongValue];
	} else {
		NSLog(@"Warning, no suitable type found.");
		return 0;
	}
#endif
}
