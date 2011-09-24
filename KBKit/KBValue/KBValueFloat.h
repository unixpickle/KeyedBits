//
//  KBValueFloat.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/23/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBValue.h"

@interface KBValueFloat : KBValue {
	
}

- (id)initWithDouble:(double)d;
- (id)initWithFloat:(float)f;

- (double)doubleValue;
- (float)floatValue;

@end
