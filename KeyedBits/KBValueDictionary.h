//
//  KBValueDictionary.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/23/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBValue.h"

@interface KBValueDictionary : KBValue {
	
}

- (id)initWithDictionary:(NSDictionary *)dictionary;

- (NSDictionary *)dictionary;

@end
