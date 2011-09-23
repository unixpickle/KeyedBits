//
//  KBValueInteger.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/23/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBValue.h"
#import "NSNumber+SInt.h"

@interface KBValueInteger : KBValue {
	
}

- (id)initWithInt32:(SInt32)myInt;

- (SInt32)intValue;

@end
