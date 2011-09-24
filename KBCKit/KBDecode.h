//
//  KBDecode.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/24/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#ifndef KeyedBits_KBDecode_h
#define KeyedBits_KBDecode_h

#include "KBContext.h"
#include "KBConst.h"

bool kb_decode_read_type (KBContextRef ctx, uint8_t * type, uint8_t * typeNumber);
char * kb_decode_string (KBContextRef ctx, uint8_t type);
void * kb_decode_data (KBContextRef ctx, uint8_t type, size_t * dataLength);
bool kb_decode_integer (KBContextRef ctx, uint8_t type, int64_t * destination);
bool kb_decode_double (KBContextRef ctx, uint8_t type, double * destination);
bool kb_decode_dictionary_key (KBContextRef ctx, uint8_t type, char ** keyString);

#endif
