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
#include <stdbool.h>
#include <unistd.h>
#include <errno.h>

struct KBContext {
	char * buffer;
	int fdesc; // A NULL buffer indicates the use of this
	size_t usedLength;
	size_t allocLength;
	size_t bufferSize;
};

typedef struct KBContext *KBContextRef;

KBContextRef kb_context_create (void);
KBContextRef kb_context_create_buffer (size_t buffer);
KBContextRef kb_context_create_data (const void * bufferData, size_t length);
KBContextRef kb_context_create_file (int fd);

// writing
void kb_context_append_bytes (KBContextRef ctx, const void * bytes, size_t length);
void kb_context_append_uint8 (KBContextRef ctx, uint8_t anInt);
void kb_context_append_uint16 (KBContextRef ctx, uint16_t anInt);
void kb_context_append_uint24 (KBContextRef ctx, uint32_t anInt);
void kb_context_append_uint32 (KBContextRef ctx, uint32_t anInt);
void kb_context_append_uint64 (KBContextRef ctx, uint64_t anInt);

// reading
bool kb_context_read_bytes (KBContextRef ctx, void * destination, size_t length);
bool kb_context_read_uint8 (KBContextRef ctx, uint8_t * anInt);
bool kb_context_read_uint16 (KBContextRef ctx, uint16_t * anInt);
bool kb_context_read_uint24 (KBContextRef ctx, uint32_t * anInt);
bool kb_context_read_uint32 (KBContextRef ctx, uint32_t * anInt);
bool kb_context_read_uint64 (KBContextRef ctx, uint64_t * anInt);
bool kb_context_read_uint (KBContextRef ctx, uint64_t * dest, size_t intLen);
bool kb_context_read_int (KBContextRef ctx, int64_t * dest, size_t intLen);

void kb_context_free (KBContextRef ctx);

#endif
