//
//  KBConst.h
//  KeyedBits
//
//  Created by Alex Nichol on 9/24/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#ifndef KeyedBits_KBConst_h
#define KeyedBits_KBConst_h

typedef enum {
	KBEncodeTypeNumberTerminator = 0,
	KBEncodeTypeNumberString = 1,
	KBEncodeTypeNumberArray = 2,
	KBEncodeTypeNumberDictionary = 3,
	KBEncodeTypeNumberNull = 4,
	KBEncodeTypeNumberData = 5,
	KBEncodeTypeNumberInteger = 6,
	KBEncodeTypeNumberFloat = 7
} KBEncodeTypeNumber;

#endif
