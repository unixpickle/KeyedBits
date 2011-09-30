//
//  KBEncodeObjC.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/24/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#include "KBEncode.h"

void kb_encode_objc_object (KBContextRef ctx, NSObject * anObject);
void kb_encode_objc_dictionary (KBContextRef ctx, NSDictionary * dictionary);
void kb_encode_objc_array (KBContextRef ctx, NSArray * array);
void kb_encode_objc_number (KBContextRef ctx, NSNumber * number);
void kb_encode_objc_string (KBContextRef ctx, NSString * string);
void kb_encode_objc_data (KBContextRef ctx, NSData * data);
void kb_encode_objc_null (KBContextRef ctx, NSNull * null);

NSData * kb_encode_full (NSObject * anObject);
BOOL kb_encode_full_fd (NSObject * anObject, int filedesc);
