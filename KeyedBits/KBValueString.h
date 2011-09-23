//
//  KBValueString.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/22/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBValue.h"

@interface KBValueString : KBValue {
	
}

- (id)initWithString:(NSString *)string;

- (NSString *)string;

@end
