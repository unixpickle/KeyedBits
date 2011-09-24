//
//  KBEncode.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/24/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#ifndef KeyedBits_KBEncode_h
#define KeyedBits_KBEncode_h

#include "KBContext.h"
#include "KBConst.h"

void kb_encode_type (KBContextRef ctx, uint8_t lenLen, uint8_t nullTerm, KBEncodeTypeNumber type);

void kb_encode_integer64 (KBContextRef ctx, int64_t anInt);
void kb_encode_integer32 (KBContextRef ctx, int32_t anInt);
void kb_encode_double (KBContextRef ctx, double d);

void kb_encode_string (KBContextRef ctx, const char * utf8String, size_t length);
void kb_encode_data (KBContextRef ctx, const void * data, size_t length);
void kb_encode_null (KBContextRef ctx);

void kb_encode_begin_dictionary (KBContextRef ctx);
void kb_encode_begin_array (KBContextRef ctx);

void kb_encode_dictionary_key (KBContextRef ctx, const char * asciiKey);

void kb_encode_end_dictionary (KBContextRef ctx);
void kb_encode_end_array (KBContextRef ctx);

#endif
