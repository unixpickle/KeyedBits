//
//  KBObjectCoder.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/22/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "KBValueString.h"
#import "KBValueData.h"
#import "KBValueArray.h"
#import "KBValueInteger.h"
#import "KBValueFloat.h"

@interface KBObjectCoder : NSObject

+ (Class)valueClassForType:(UInt8)type;
+ (Class)valueClassForKeyedData:(NSData *)data;
+ (Class)valueClassForObject:(NSObject *)anObject;

@end
