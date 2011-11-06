//
//  KBDecodeObjC.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/24/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBDecodeObjC.h"

NSObject * kb_decode_objc_type_object (KBContextRef ctx) {
	uint8_t type = 0;
	if (!kb_decode_read_type(ctx, &type, NULL)) {
		return nil;
	}
	return kb_decode_objc_object(ctx, type);
}

NSObject * kb_decode_objc_object (KBContextRef ctx, uint8_t type) {
	uint8_t typeNumber = type & 7;
	switch (typeNumber) {
		case KBEncodeTypeNumberData:
			return kb_decode_objc_data(ctx, type);
		case KBEncodeTypeNumberArray:
			return kb_decode_objc_array(ctx, type);
		case KBEncodeTypeNumberDictionary:
			return kb_decode_objc_dictionary(ctx, type);
		case KBEncodeTypeNumberFloat:
			return kb_decode_objc_double(ctx, type);
		case KBEncodeTypeNumberInteger:
			return kb_decode_objc_integer(ctx, type);
		case KBEncodeTypeNumberNull:
			return kb_decode_objc_null(ctx, type);
		case KBEncodeTypeNumberString:
			return kb_decode_objc_string(ctx, type);
		default:
			break;
	}
	return nil;
}

NSArray * kb_decode_objc_array (KBContextRef ctx, uint8_t type) {
	NSMutableArray * array = [[NSMutableArray alloc] init];
	while (true) {
		uint8_t typeSub = 0;
		uint8_t typeNum = 0;
		// read an type, then the object.
		if (!kb_decode_read_type(ctx, &typeSub, &typeNum)) {
#if !__has_feature(objc_arc)
			[array release];
#endif
			return nil;
		}
		if (typeSub == 0) break;
		NSObject * nextObject = kb_decode_objc_object(ctx, typeSub);
		if (!nextObject) {
#if !__has_feature(objc_arc)
			[array release];
#endif
			return nil;
		}
		[array addObject:nextObject];
	}
	// If I were to create an immutable array enclosing this, it
	// would take a few more milliseconds... no thanks.
#if !__has_feature(objc_arc)
	return [array autorelease];
#else
	return array;
#endif
}

NSDictionary * kb_decode_objc_dictionary (KBContextRef ctx, uint8_t type) {
	NSMutableDictionary * dictionary = [[NSMutableDictionary alloc] init];
	while (true) {
		uint8_t type = 0;
		uint8_t typeNum = 0;
		char * keyStr = NULL;
		if (!kb_decode_dictionary_key(ctx, type, &keyStr)) {
#if !__has_feature(objc_arc)
			[dictionary release];
#endif
			return nil;
		}
		if (!keyStr) break;
		NSString * objKey = [[NSString alloc] initWithBytesNoCopy:keyStr
														   length:strlen(keyStr)
														 encoding:NSASCIIStringEncoding
													 freeWhenDone:YES];
		if (!objKey) {
			free(keyStr);
#if !__has_feature(objc_arc)
			[dictionary release];
#endif
			return nil;
		}
		// read an type, then the object.
		if (!kb_decode_read_type(ctx, &type, &typeNum)) {
#if !__has_feature(objc_arc)
			[objKey release];
			[dictionary release];
#endif
			return nil;
		}
		NSObject * nextObject = kb_decode_objc_object(ctx, type);
		if (!nextObject) {
#if !__has_feature(objc_arc)
			[objKey release];
			[dictionary release];
#endif
			return nil;
		}
		[dictionary setObject:nextObject forKey:objKey];
#if !__has_feature(objc_arc)
		[objKey release];
#endif
	}
	// If I were to create an immutable dictionary enclosing this, it
	// would take a few more milliseconds... no thanks.
#if !__has_feature(objc_arc)
	return [dictionary autorelease];
#else
	return dictionary;
#endif
}

NSString * kb_decode_objc_string (KBContextRef ctx, uint8_t type) {
	char * buffer = NULL;
	if ((buffer = kb_decode_string(ctx, type)) == NULL) {
		return nil;
	}
	NSString * string = [[NSString alloc] initWithBytesNoCopy:buffer
													   length:strlen(buffer)
													 encoding:NSUTF8StringEncoding
												 freeWhenDone:YES];
	if (!string) {
		free(buffer);
	}
#if !__has_feature(objc_arc)
	return [string autorelease];
#else
	return string;
#endif
}

NSNumber * kb_decode_objc_integer (KBContextRef ctx, uint8_t type) {
	int64_t number = 0;
	if (!kb_decode_integer(ctx, type, &number)) {
		return nil;
	}
	
#if __WORDSIZE == 64
	return [NSNumber numberWithLong:number];
#elif __WORDSIZE == 32
	return [NSNumber numberWithLongLong:number];
#else
	if (sizeof(int) == sizeof(int64_t)) {
		return [NSNumber numberWithInt:(int)number];
	} else if (sizeof(long) == sizeof(int64_t)) {
		return [NSNumber numberWithLong:(long)number];
	} else if (sizeof(long long) == sizeof(int64_t)) {
		return [NSNumber numberWithLongLong:(long long)number];
	} else {
		NSLog(@"Warning, no suitable type found.");
		return nil;
	}
#endif
}

NSNumber * kb_decode_objc_double (KBContextRef ctx, uint8_t type) {
	double d = 0;
	if (!kb_decode_double(ctx, type, &d)) {
		return nil;
	}
	return [NSNumber numberWithDouble:d];
}

NSNull * kb_decode_objc_null (KBContextRef ctx, uint8_t type) {
	return [NSNull null];
}

NSData * kb_decode_objc_data (KBContextRef ctx, uint8_t type) {
	char * buffer;
	size_t length;
	if ((buffer = kb_decode_data(ctx, type, &length)) == NULL) {
		return nil;
	}
	return [NSData dataWithBytesNoCopy:buffer length:length freeWhenDone:YES];
}

NSObject * kb_decode_full (NSData * data) {
	KBContextRef ctx = kb_context_create_data([data bytes], [data length]);
	NSObject * root = kb_decode_objc_type_object(ctx);
	kb_context_free(ctx);
	return root;
}

NSObject * kb_decode_full_fd (int filedesc) {
	KBContextRef ctx = kb_context_create_file(filedesc);
	NSObject * root = kb_decode_objc_type_object(ctx);
	kb_context_free(ctx);
	return root;
}
