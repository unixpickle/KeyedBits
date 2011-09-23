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
	NSObject * decoded = [NSString objectWithKeyedBitsData:encoded];
	NSLog(@"%@", decoded);
	
	NSData * encodeMe = [@"Hello, ASCII data!" dataUsingEncoding:NSASCIIStringEncoding];
	encoded = [[encodeMe keyedBitsValue] encodeValue];
	decoded = [NSData objectWithKeyedBitsData:encoded];
	NSString * message = [[NSString alloc] initWithData:(NSData *)decoded encoding:NSASCIIStringEncoding];
	NSLog(@"%@", message);
	[message release];
	
	[pool drain];
    return 0;
}

