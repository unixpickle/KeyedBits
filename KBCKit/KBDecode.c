//
//  KBDecode.c
//  KeyedBits
//
//  Created by Alex Nichol on 9/24/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#include "KBDecode.h"

static bool _kb_decode_read_value (KBContextRef ctx, uint8_t type, char ** buffer, size_t * buffLen);

bool kb_decode_read_type (KBContextRef ctx, uint8_t * type, uint8_t * typeNumber) {
	if (!kb_context_read_uint8(ctx, type)) {
		return false;
	}
	if (typeNumber != NULL) {
		*typeNumber = (*type & 7);
	}
	return true;
}

char * kb_decode_string (KBContextRef ctx, uint8_t type) {
	char * buffer = NULL;
	size_t length = 0;
	if (!_kb_decode_read_value(ctx, type, &buffer, &length)) {
		return NULL;
	}
	return buffer;
}

void * kb_decode_data (KBContextRef ctx, uint8_t type, size_t * dataLength) {
	char * buffer = NULL;
	size_t length = 0;
	if (!_kb_decode_read_value(ctx, type, &buffer, &length)) {
		return NULL;
	}
	*dataLength = length;
	return buffer;
}

bool kb_decode_integer (KBContextRef ctx, uint8_t type, int64_t * destination) {
	uint8_t lenLength = 1;
	// detect byte order
	if ((lenLength << 1) == 2) {
		lenLength = (type >> 5) & 3;
	} else {
		lenLength = (type << 5) & 3;
	}
	lenLength += 1;
	lenLength = (1 << lenLength);
	return kb_context_read_int(ctx, destination, lenLength);
}

bool kb_decode_double (KBContextRef ctx, uint8_t type, double * destination) {
	char * str;
	size_t strLen;
	if (!_kb_decode_read_value(ctx, type, &str, &strLen)) {
		return false;
	}
	*destination = atof(str);
	free(str);
	return true;
}

bool kb_decode_dictionary_key (KBContextRef ctx, uint8_t type, char ** keyString) {
	char * string = (char *)malloc(32);
	size_t bytesRead = 0;
	size_t bytesTotal = 32;
	while (true) {
		uint8_t aChar = 0;
		uint8_t cleanChar = 0;
		if (!kb_context_read_uint8(ctx, &aChar)) {
			free(string);
			return false;
		}
		if (aChar == 0 && bytesRead == 0) {
			*keyString = NULL;
			free(string);
			return true;
		}
		cleanChar = aChar;
		if ((cleanChar & 128) != 0) {
			cleanChar ^= 128;
		}
		string[bytesRead++] = cleanChar;
		if (aChar == 0) {
			free(string);
			return false;
		}
		if (cleanChar != aChar) {
			string[bytesRead++] = 0;
			break;
		}
		
		// ensure that there are always 2 bytes loose
		if (bytesRead + 1 == bytesTotal) {
			bytesTotal += 32;
			string = realloc(string, bytesTotal);
		}
	}
	*keyString = string;
	return true;
}

// Private

static bool _kb_decode_read_value (KBContextRef ctx, uint8_t type, char ** buffer, size_t * buffLen) {
	if ((type & 128) == 0) {
		uint8_t lenLength = 1;
		uint64_t length = 0;
		// detect byte order
		if ((lenLength << 1) == 2) {
			lenLength = (type >> 5) & 3;
		} else {
			lenLength = (type << 5) & 3;
		}
		lenLength += 1;
		if (!kb_context_read_uint(ctx, &length, lenLength)) {
			return false;
		}
		*buffer = (char *)malloc((size_t)length);
		*buffLen = (size_t)length;
		if (!kb_context_read_bytes(ctx, *buffer, (size_t)length)) {
			free(*buffer);
			return false;
		}
		return true;
	} else {
		// read NULL terminated buffer
		char * string = (char *)malloc(512);
		size_t bytesRead = 0;
		size_t bytesTotal = 512;
		while (true) {
			uint8_t aChar = 0;
			if (!kb_context_read_uint8(ctx, &aChar)) {
				free(string);
				return NULL;
			}
			string[bytesRead++] = aChar;
			if (aChar == 0) {
				break;
			}
			if (bytesRead == bytesTotal) {
				bytesTotal += 512;
				string = realloc(string, bytesTotal);
			}
		}
		*buffLen = bytesRead;
		*buffer = string;
		return true;
	}
	return false;
}
