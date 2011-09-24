//
//  KBContext.c
//  KeyedBits
//
//  Created by Alex Nichol on 9/24/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#include "KBContext.h"

static int _KBContextIsBigEndian ();

KBContextRef kb_context_create (void) {
	KBContextRef ctx = (KBContextRef)malloc(sizeof(struct KBContext));
	ctx->bufferSize = 1024;
	ctx->buffer = (char *)malloc(ctx->bufferSize);
	ctx->usedLength = 0;
	ctx->allocLength = ctx->bufferSize;
	return ctx;
}

KBContextRef kb_context_create_buffer (size_t buffer) {
	KBContextRef ctx = (KBContextRef)malloc(sizeof(struct KBContext));
	ctx->bufferSize = buffer;
	ctx->buffer = (char *)malloc(ctx->bufferSize);
	ctx->usedLength = 0;
	ctx->allocLength = ctx->bufferSize;
	return ctx;
}

void kb_context_append_bytes (KBContextRef ctx, const void * bytes, size_t length) {
	if (ctx->usedLength + length > ctx->allocLength) {
		size_t totalSize = ctx->usedLength + length;
		size_t remaining = (totalSize % ctx->bufferSize);
		if (remaining != 0) {
			totalSize += ctx->bufferSize - remaining;
		}
		ctx->buffer = realloc(ctx->buffer, totalSize);
		ctx->allocLength = totalSize;
	}
	memcpy(&ctx->buffer[ctx->usedLength], bytes, length);
	ctx->usedLength += length;
}

void kb_context_append_uint8 (KBContextRef ctx, uint8_t anInt) {
	kb_context_append_bytes(ctx, &anInt, 1);
}

void kb_context_append_uint16 (KBContextRef ctx, uint16_t anInt) {
	uint16_t little;
	if (_KBContextIsBigEndian()) {
		char * buffer = (char *)&little;
		const char * source = (const char *)&anInt;
		buffer[0] = source[1];
		buffer[1] = source[0];
	}
	kb_context_append_bytes(ctx, &little, 2);
}

void kb_context_append_uint24 (KBContextRef ctx, uint32_t anInt) {
	uint32_t little = anInt;
	if (_KBContextIsBigEndian()) {
		char * buffer = (char *)&little;
		const char * source = (const char *)&anInt;
		int i;
		for (i = 0; i < 4; i++) {
			buffer[i] = source[3 - i];
		}
	}
	kb_context_append_bytes(ctx, &little, 3);
}

void kb_context_append_uint32 (KBContextRef ctx, uint32_t anInt) {
	uint32_t little = anInt;
	if (_KBContextIsBigEndian()) {
		char * buffer = (char *)&little;
		const char * source = (const char *)&anInt;
		int i;
		for (i = 0; i < 4; i++) {
			buffer[i] = source[3 - i];
		}
	}
	kb_context_append_bytes(ctx, &little, 4);
}

void kb_context_append_uint64 (KBContextRef ctx, uint64_t anInt) {
	uint64_t little = anInt;
	if (_KBContextIsBigEndian()) {
		char * buffer = (char *)&little;
		const char * source = (const char *)&anInt;
		int i;
		for (i = 0; i < 8; i++) {
			buffer[i] = source[7 - i];
		}
	}
	kb_context_append_bytes(ctx, &little, 8);
}

void kb_context_free (KBContextRef ctx) {
	free(ctx->buffer);
	free(ctx);
}

static int _KBContextIsBigEndian () {
	uint32_t myInt = 1;
	char * buffer = (char *)&myInt;
	if (buffer[0] == 1) return 0;
	return 1;
}
