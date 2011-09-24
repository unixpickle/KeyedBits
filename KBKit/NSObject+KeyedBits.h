//
//  NSObject+KeyedBits.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/22/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "KBObjectCoder.h"

@interface NSObject (KeyedBits)

+ (id)objectWithKeyedBitsData:(NSData *)theData;
- (KBValue *)keyedBitsValue;

@end
