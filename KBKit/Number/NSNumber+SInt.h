//
//  NSNumber+SInt.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/23/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface NSNumber (SInt)

+ (NSNumber *)numberWithSInt32:(SInt32)signedInt32;
+ (NSNumber *)numberWithSInt64:(SInt64)signedInt64;
- (SInt32)SInt32Value;
- (SInt64)SInt64Value;
- (BOOL)intNeeds64Bytes;
- (BOOL)isInteger;

@end
