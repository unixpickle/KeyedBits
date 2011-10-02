//
//  KBContext.c
//  KeyedBits
//
//  Created by Alex Nichol on 9/24/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#include "KBContext.h"

static int _KBContextIsBigEndian ();
static void _kb_context_socket_write (KBContextRef ctx, const void * bytes, size_t length);
static bool _kb_context_socket_read (KBContextRef ctx, void * destination, size_t length);

KBContextRef kb_context_create (void) {
	KBContextRef ctx = (KBContextRef)malloc(sizeof(struct KBContext));
	ctx->bufferSize = 1024;
	ctx->buffer = (char *)malloc(ctx->bufferSize);
	ctx->usedLength = 0;
	ctx->allocLength = ctx->bufferSize;
	return ctx;
}

KBContextRef kb_context_create_buffer (size_t buffer) {
	if (buffer == 0) return NULL;
	KBContextRef ctx = (KBContextRef)malloc(sizeof(struct KBContext));
	ctx->bufferSize = buffer;
	ctx->buffer = (char *)malloc(ctx->bufferSize);
	ctx->usedLength = 0;
	ctx->allocLength = ctx->bufferSize;
	return ctx;
}

KBContextRef kb_context_create_data (const void * bufferData, size_t length) {
	KBContextRef ctx = (KBContextRef)malloc(sizeof(struct KBContext));
	ctx->bufferSize = 0;
	ctx->buffer = (char *)bufferData;
	ctx->usedLength = 0;
	ctx->allocLength = length;
	return ctx;
}

KBContextRef kb_context_create_file (int fd) {
	KBContextRef ctx = (KBContextRef)malloc(sizeof(struct KBContext));
	ctx->bufferSize = 512;
	ctx->buffer = NULL;
	ctx->fdesc = fd;
	ctx->usedLength = 0;
	ctx->allocLength = 0;
	return ctx;
}

// Writing

void kb_context_append_bytes (KBContextRef ctx, const void * bytes, size_t length) {
	if (!ctx->buffer) {
		_kb_context_socket_write(ctx, bytes, length);
		return;
	}
	if (ctx->bufferSize == 0) return;
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
	uint16_t little = anInt;
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
		buffer[2] = source[0];
		buffer[1] = source[1];
		buffer[0] = source[2];
	}
	kb_context_append_bytes(ctx, &little, 3);
}

void kb_context_append_uint32 (KBContextRef ctx, uint32_t anInt) {
	uint32_t little = anInt;
	if (_KBContextIsBigEndian()) {
		char * buffer = (char *)&little;
		const char * source = (const char *)&anInt;
		buffer[0] = source[3];
		buffer[1] = source[2];
		buffer[2] = source[1];
		buffer[3] = source[0];
	}
	kb_context_append_bytes(ctx, &little, 4);
}

void kb_context_append_uint64 (KBContextRef ctx, uint64_t anInt) {
	uint64_t little = anInt;
	if (_KBContextIsBigEndian()) {
		char * buffer = (char *)&little;
		const char * source = (const char *)&anInt;
		buffer[0] = source[7];
		buffer[1] = source[6];
		buffer[2] = source[5];
		buffer[3] = source[4];
		buffer[4] = source[3];
		buffer[5] = source[2];
		buffer[6] = source[1];
		buffer[7] = source[0];
	}
	kb_context_append_bytes(ctx, &little, 8);
}

// Reading

bool kb_context_read_bytes (KBContextRef ctx, void * destination, size_t length) {
	if (ctx->buffer == NULL) {
		return _kb_context_socket_read(ctx, destination, length);
	}
	if (length + ctx->usedLength > ctx->allocLength) {
		return false;
	}
	memcpy(destination, &ctx->buffer[ctx->usedLength], length);
	ctx->usedLength += length;
	return true;
}

bool kb_context_read_uint8 (KBContextRef ctx, uint8_t * anInt) {
	return kb_context_read_bytes(ctx, anInt, 1);
}

bool kb_context_read_uint16 (KBContextRef ctx, uint16_t * anInt) {
	if (!kb_context_read_bytes(ctx, anInt, 2)) {
		return false;
	}
	if (_KBContextIsBigEndian()) {
		uint16_t lendian = *anInt;
		char * destination = (char *)anInt;
		const char * source = (const char *)&lendian;
		destination[0] = source[1];
		destination[1] = source[0];
	}
	return true;
}

bool kb_context_read_uint24 (KBContextRef ctx, uint32_t * anInt) {
	if (!kb_context_read_bytes(ctx, anInt, 3)) {
		return false;
	}
	if (_KBContextIsBigEndian()) {
		uint32_t lendian = *anInt;
		char * destination = (char *)anInt;
		const char * source = (const char *)&lendian;
		destination[1] = source[2];
		destination[2] = source[1];
		destination[3] = source[0];
		destination[0] = 0;
	}
	return true;
}

bool kb_context_read_uint32 (KBContextRef ctx, uint32_t * anInt) {
	if (!kb_context_read_bytes(ctx, anInt, 4)) {
		return false;
	}
	if (_KBContextIsBigEndian()) {
		uint32_t lendian = *anInt;
		char * destination = (char *)anInt;
		const char * source = (const char *)&lendian;
		destination[0] = source[3];
		destination[1] = source[2];
		destination[2] = source[1];
		destination[3] = source[0];
	}
	return true;
}

bool kb_context_read_uint64 (KBContextRef ctx, uint64_t * anInt) {
	if (!kb_context_read_bytes(ctx, anInt, 8)) {
		return false;
	}
	if (_KBContextIsBigEndian()) {
		uint64_t lendian = *anInt;
		char * destination = (char *)anInt;
		const char * source = (const char *)&lendian;
		destination[0] = source[7];
		destination[1] = source[6];
		destination[2] = source[5];
		destination[3] = source[4];
		destination[4] = source[3];
		destination[5] = source[2];
		destination[6] = source[1];
		destination[7] = source[0];
	}
	return true;
}

bool kb_context_read_uint (KBContextRef ctx, uint64_t * dest, size_t intLen) {
	if (intLen == 1) {
		uint8_t small = 0;
		if (kb_context_read_uint8(ctx, &small)) {
			*dest = (uint64_t)small;
			return true;
		}
	} else if (intLen == 2) {
		uint16_t small = 0;
		if (kb_context_read_uint16(ctx, &small)) {
			*dest = (uint64_t)small;
			return true;
		}
	} else if (intLen == 3) {
		uint32_t small = 0;
		if (kb_context_read_uint24(ctx, &small)) {
			*dest = (uint64_t)small;
			return true;
		}
	} else if (intLen == 4) {
		uint32_t small = 0;
		if (kb_context_read_uint32(ctx, &small)) {
			*dest = (uint64_t)small;
			return true;
		}
	} else if (intLen == 8) {
		uint64_t small = 0;
		if (kb_context_read_uint64(ctx, &small)) {
			*dest = small;
			return true;
		}
	}
	return false;
}

bool kb_context_read_int (KBContextRef ctx, int64_t * dest, size_t intLen) {
	if (intLen == 1) {
		uint8_t small = 0;
		if (!kb_context_read_uint8(ctx, &small)) {
			*dest = (int8_t)small;
			return true;
		}
	} else if (intLen == 2) {
		uint16_t small = 0;
		if (kb_context_read_uint16(ctx, &small)) {
			*dest = (int16_t)small;
			return true;
		}
	} else if (intLen == 4) {
		uint32_t small = 0;
		if (kb_context_read_uint32(ctx, &small)) {
			*dest = (int32_t)small;
			return true;
		}
	} else if (intLen == 8) {
		uint64_t small = 0;
		if (kb_context_read_uint64(ctx, &small)) {
			*dest = (int64_t)small;
			return true;
		}
	}
	return false;
}

// Freeing

void kb_context_free (KBContextRef ctx) {
	if (ctx->bufferSize && ctx->buffer != NULL) free(ctx->buffer);
	free(ctx);
}

// Private

static int _KBContextIsBigEndian () {
	static bool hasFound = false;
	static int answer = 0;
	if (!hasFound) {
		uint32_t myInt = 1;
		char * buffer = (char *)&myInt;
		if (buffer[0] == 1) answer = 0;
		else answer = 1;
		hasFound = true;
	}
	return answer;
}

static void _kb_context_socket_write (KBContextRef ctx, const void * bytes, size_t length) {
	if (ctx->fdesc < 0) return;
	size_t written = 0;
	while (written < length) {
		size_t toWrite = length - written;
		if (toWrite > ctx->bufferSize) {
			toWrite = ctx->bufferSize;
		}
		ssize_t wrote = write(ctx->fdesc, &bytes[written], toWrite);
		if (wrote <= 0) {
			if (errno != EINTR) {
				ctx->fdesc = -1;
				return;
			}
		} else {
			written += wrote;
		}
	}
}

static bool _kb_context_socket_read (KBContextRef ctx, void * _destination, size_t length) {
	if (ctx->fdesc < 0) return false;
	char * destination = (char *)_destination;
	size_t bytesRead = 0;
	while (bytesRead < length) {
		size_t toRead = ctx->bufferSize;
		if (toRead > length - bytesRead) {
			toRead = length - bytesRead;
		}
		ssize_t justRead = read(ctx->fdesc, &destination[bytesRead], toRead);
		if (justRead <= 0) {
			if (errno != EINTR) {
				ctx->fdesc = -1;
				return false;
			}
		} else {
			bytesRead = justRead;
		}
	}
	return true;
}
