//
//  KBContext.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/24/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#ifndef KeyedBits_KBContext_h
#define KeyedBits_KBContext_h

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

struct KBContext {
	char * buffer;
	size_t usedLength;
	size_t allocLength;
	size_t bufferSize;
};

typedef struct KBContext *KBContextRef;

KBContextRef kb_context_create (void);
KBContextRef kb_context_create_buffer (size_t buffer);

// writing
void kb_context_append_bytes (KBContextRef ctx, const void * bytes, size_t length);
void kb_context_append_uint8 (KBContextRef ctx, uint8_t anInt);
void kb_context_append_uint16 (KBContextRef ctx, uint16_t anInt);
void kb_context_append_uint24 (KBContextRef ctx, uint32_t anInt);
void kb_context_append_uint32 (KBContextRef ctx, uint32_t anInt);
void kb_context_append_uint64 (KBContextRef ctx, uint64_t anInt);

void kb_context_free (KBContextRef ctx);

#endif
