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

@interface KBObjectCoder : NSObject

+ (Class)valueClassForKeyedData:(NSData *)data;
+ (Class)valueClassForObject:(NSObject *)anObject;

@end
