//
//  main.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/22/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "NSObject+KeyedBits.h"

void TestString (void);
void TestData (void);
void TestArray (void);
void TestInteger (void);

int main (int argc, const char * argv[]) {
	NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	
	TestString();
	TestData();
	TestArray();
	TestInteger();
	
	[pool drain];
    return 0;
}

void TestString (void) {
	NSData * encoded = [[@"Hello, KeyedBits!" keyedBitsValue] encodeValue];
	NSString * decoded = [NSString objectWithKeyedBitsData:encoded];
	NSLog(@"%@", decoded);
}

void TestData (void) {
	NSData * encodeMe = [@"Hello, ASCII data!" dataUsingEncoding:NSASCIIStringEncoding];
	NSData * encoded = [[encodeMe keyedBitsValue] encodeValue];
	NSData * decoded = [NSData objectWithKeyedBitsData:encoded];
	NSString * message = [[NSString alloc] initWithData:(NSData *)decoded encoding:NSASCIIStringEncoding];
	NSLog(@"%@", message);
	[message release];
}

void TestArray (void) {
	NSArray * messages = [NSArray arrayWithObjects:@"This", @"Is", @"A", @"Test", nil];
	NSData * encoded = [[messages keyedBitsValue] encodeValue];
	NSArray * decoded = [NSArray objectWithKeyedBitsData:encoded];
	NSLog(@"%@", decoded);
}

void TestInteger (void) {
	NSNumber * integer = [NSNumber numberWithInt:15];
	NSNumber * negative = [NSNumber numberWithInt:-1337];
	NSData * encoded = [[integer keyedBitsValue] encodeValue];
	NSNumber * decoded = [NSNumber objectWithKeyedBitsData:encoded];
	NSLog(@"15 = %@", decoded);
	encoded = [[negative keyedBitsValue] encodeValue];
	decoded = [NSNumber objectWithKeyedBitsData:encoded];
	NSLog(@"-1337 = %@", decoded);
}

