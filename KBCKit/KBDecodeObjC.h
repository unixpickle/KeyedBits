//
//  KBDecodeObjC.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/24/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#include "KBDecode.h"

NSObject * kb_decode_objc_type_object (KBContextRef ctx);
NSObject * kb_decode_objc_object (KBContextRef ctx, uint8_t type);
NSArray * kb_decode_objc_array (KBContextRef ctx, uint8_t type);
NSDictionary * kb_decode_objc_dictionary (KBContextRef ctx, uint8_t type);
NSString * kb_decode_objc_string (KBContextRef ctx, uint8_t type);
NSNumber * kb_decode_objc_integer (KBContextRef ctx, uint8_t type);
NSNumber * kb_decode_objc_double (KBContextRef ctx, uint8_t type);
NSNull * kb_decode_objc_null (KBContextRef ctx, uint8_t type);
NSData * kb_decode_objc_data (KBContextRef ctx, uint8_t type);

NSObject * kb_decode_full (NSData * data);
NSObject * kb_decode_full_fd (int filedesc);
