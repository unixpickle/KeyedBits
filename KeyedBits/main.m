//
//  main.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/22/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "NSObject+KeyedBits.h"

int main (int argc, const char * argv[]) {
	NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	
	NSData * encoded = [[@"Hello, KeyedBits!" keyedBitsValue] encodeValue];
	NSString * decoded = [NSString objectWithKeyedBitsData:encoded];
	NSLog(@"%@", decoded);
	
	[pool drain];
    return 0;
}

