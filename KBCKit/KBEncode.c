//
//  KBEncode.c
//  KeyedBits
//
//  Created by Alex Nichol on 9/24/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#include "KBEncode.h"

void kb_encode_type (KBContextRef ctx, uint8_t lenLen, uint8_t nullTerm, KBEncodeTypeNumber type) {
	uint8_t typeNum = 1;
	uint8_t lenLenLarge = (lenLen - 1) & 3;
	// detect bit order
	if ((typeNum << 1) == 2) {
		typeNum = 0;
		typeNum |= ((nullTerm & 1) << 7);
		typeNum |= ((lenLenLarge & 3) << 5);
		typeNum |= (type & 7);
	} else {
		typeNum = 0;
		typeNum |= ((nullTerm & 1) >> 7);
		typeNum |= ((lenLenLarge & 3) >> 5);
		typeNum |= (type & 7);
	}
	kb_context_append_uint8(ctx, typeNum);
}

void kb_encode_integer64 (KBContextRef ctx, int64_t anInt) {
	kb_encode_type(ctx, 3, 1, KBEncodeTypeNumberInteger);
	kb_context_append_uint64(ctx, anInt);
}

void kb_encode_integer32 (KBContextRef ctx, int32_t anInt) {
	kb_encode_type(ctx, 2, 1, KBEncodeTypeNumberInteger);
	kb_context_append_uint32(ctx, anInt);
}

void kb_encode_double (KBContextRef ctx, double d) {
	char dString[64];
	sprintf(dString, "%lf", d);
	kb_encode_type(ctx, 1, 1, KBEncodeTypeNumberFloat);
	kb_context_append_bytes(ctx, dString, strlen(dString) + 1);
}

void kb_encode_string (KBContextRef ctx, const char * utf8String, size_t length) {
	kb_encode_type(ctx, 1, 1, KBEncodeTypeNumberString);
	kb_context_append_bytes(ctx, utf8String, strlen(utf8String) + 1);
}

void kb_encode_data (KBContextRef ctx, const void * data, size_t length) {
	if (length <= 0xff) {
		kb_encode_type(ctx, 1, 0, KBEncodeTypeNumberData);
		kb_context_append_uint8(ctx, (uint8_t)length);
	} else if (length <= 0xffff) {
		kb_encode_type(ctx, 2, 0, KBEncodeTypeNumberData);
		kb_context_append_uint16(ctx, (uint16_t)length);
	} else if (length <= 0xffffff) {
		kb_encode_type(ctx, 3, 0, KBEncodeTypeNumberData);
		kb_context_append_uint24(ctx, (uint32_t)length);
	} else if (length <= 0xffffffff) {
		kb_encode_type(ctx, 4, 0, KBEncodeTypeNumberData);
		kb_context_append_uint32(ctx, (uint32_t)length);
	} else {
		return;
	}
	kb_context_append_bytes(ctx, data, length);
}

void kb_encode_null (KBContextRef ctx) {
	kb_encode_type(ctx, 1, 1, KBEncodeTypeNumberNull);
}

// Collections (begin)

void kb_encode_begin_dictionary (KBContextRef ctx) {
	kb_encode_type(ctx, 1, 1, KBEncodeTypeNumberDictionary);
}

void kb_encode_begin_array (KBContextRef ctx) {
	kb_encode_type(ctx, 1, 1, KBEncodeTypeNumberArray);
}

// Collections (body)

void kb_encode_dictionary_key (KBContextRef ctx, const char * asciiKey) {
	size_t length = strlen(asciiKey);
	uint8_t termFlag = 128;
	if (length == 0) {
		kb_context_append_uint8(ctx, termFlag);
	} else {
		size_t i;
		for (i = 0; i < length; i++) {
			uint8_t token = asciiKey[i];
			if ((token & termFlag) != 0) {
				token ^= termFlag;
			}
			if (i + 1 == length) {
				token |= termFlag;
			}
			kb_context_append_uint8(ctx, token);
		}
	}
}

// Collections (end)

void kb_encode_end_dictionary (KBContextRef ctx) {
	kb_context_append_uint8(ctx, 0);
}

void kb_encode_end_array (KBContextRef ctx) {
	kb_context_append_uint8(ctx, 0);
}
