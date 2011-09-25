//
//  main.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/22/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "NSObject+KeyedBits.h"
#import "NSObject+SBJson.h"
#import "KBEncodeObjC.h"
#import "KBDecodeObjC.h"

void TestString (void);
void TestData (void);
void TestArray (void);
void TestInteger (void);
void TestFloating (void);
void TestDictionary (void);
void TestCDictionary (void);
void TestCLargeData (void);
void Benchmark (void);

int main (int argc, const char * argv[]) {
	NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	
	TestString();
	TestData();
	TestInteger();
	TestFloating();
	TestArray();
	TestDictionary();
	TestCDictionary();
	TestCLargeData();
	Benchmark();
	
	[pool drain];
	
	while (true) {
		sleep(1);
	}
	
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
	
	NSArray * mixed = [NSArray arrayWithObjects:[NSNumber numberWithInt:17],
					   [NSData dataWithBytes:"\x02\x00\x01" length:3],
												@"Foobar",
												messages,
												[NSNull null],
												[NSNumber numberWithDouble:3.1415],
												nil];
	encoded = [[mixed keyedBitsValue] encodeValue];
	decoded = [NSArray objectWithKeyedBitsData:encoded];
	NSLog(@"%@", decoded);
	NSCAssert([mixed isEqual:decoded], @"Must decode to equal array");
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

void TestFloating (void) {
	NSNumber * aNumber = [NSNumber numberWithDouble:13.37];
	NSData * encoded = [[aNumber keyedBitsValue] encodeValue];
	NSNumber * decoded = [NSNumber objectWithKeyedBitsData:encoded];
	NSLog(@"13.37 = %@", decoded);
}

void TestDictionary (void) {
	NSDictionary * dog = [NSDictionary dictionaryWithObjectsAndKeys:@"Binary", @"name", 
						  @"Goldendoodle", @"breed", nil];
	NSArray * pets = [NSArray arrayWithObjects:dog, nil];
	NSDictionary * education = [NSDictionary dictionaryWithObjectsAndKeys:[NSNumber numberWithBool:YES], @"High School",
								[NSNumber numberWithBool:NO], @"College", @"Private", @"High School Type", nil];
	NSDictionary * dictionary = [NSDictionary dictionaryWithObjectsAndKeys:@"Alex", @"name",
								 [NSNumber numberWithInt:14], @"age", 
								 education, @"education", pets, @"pets", nil];
	NSData * encoded = [[dictionary keyedBitsValue] encodeValue];
	NSDictionary * decoded = [NSDictionary objectWithKeyedBitsData:encoded];
	NSLog(@"%@", decoded);
	NSCAssert([decoded isEqualToDictionary:dictionary], @"Must decode to equal dictionary");
}

void TestCDictionary (void) {
	NSDictionary * dog = [NSDictionary dictionaryWithObjectsAndKeys:@"Binary", @"name", 
						  @"Goldendoodle", @"breed", nil];
	NSArray * pets = [NSArray arrayWithObjects:dog, nil];
	NSDictionary * education = [NSDictionary dictionaryWithObjectsAndKeys:[NSNumber numberWithBool:YES], @"High School",
								[NSNumber numberWithBool:NO], @"College", @"Private", @"High School Type", nil];
	NSDictionary * dictionary = [NSDictionary dictionaryWithObjectsAndKeys:@"Alex", @"name",
								 [NSNumber numberWithInt:14], @"age", 
								 education, @"education", pets, @"pets", nil];
	NSData * encoded = kb_encode_full(dictionary);
	NSDictionary * decoded = (NSDictionary *)kb_decode_full(encoded);
	NSLog(@"%@", decoded);
	NSCAssert([decoded isEqualToDictionary:dictionary], @"Must decode to equal dictionary");
}

void TestCLargeData (void) {
	NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	NSMutableData * data = [[NSMutableData alloc] init];
	srand((int)time(NULL));
	for (int i = 0; i < 9000000; i++) {
		UInt8 r = (UInt8)rand();
		[data appendBytes:&r length:1];
	}
	NSDictionary * dictionary = [NSDictionary dictionaryWithObject:data forKey:@"BigData"];
	NSData * encoded = kb_encode_full(dictionary);
	NSDictionary * decoded = (NSDictionary *)kb_decode_full(encoded);
	NSCAssert([[decoded objectForKey:@"BigData"] isEqualToData:data], @"Must decode to equal dictionary");
	[data release];
	[pool drain];
}

void Benchmark (void) {
	NSData * benchmarkFile = [NSData dataWithContentsOfFile:@"./benchmark.json"];
	if (!benchmarkFile) {
		NSLog(@"Failed to read benchmark file.");
		return;
	}
	NSString * benchmarkString = [[NSString alloc] initWithData:benchmarkFile encoding:NSUTF8StringEncoding];
	if (!benchmarkString) {
		NSLog(@"Failed to read benchmark file.");
		return;
	}
	NSDictionary * benchmark = [benchmarkString JSONValue];
	[benchmarkString release];
	
	if (!benchmark) {
		NSLog(@"Failed to parse benchmark.");
		return;
	}
	
	NSInteger keyedBitsSize = 0;
	NSInteger jsonSize = 0;
	
	NSLog(@"Begin benchmark (JSON framework)");
	NSDate * start = [NSDate date];
	for (int i = 0; i < 200; i++) {
		NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
		NSString * encoded = [benchmark JSONRepresentation];
		NSDictionary * decoded = [encoded JSONValue];
		if (!decoded) {
			NSLog(@"Decode/encode of benchmark failed.");
			[pool drain];
			return;
		}
		jsonSize = [encoded length];
		[pool drain];
	}
	NSLog(@"Benchmark complete: %lf", [[NSDate date] timeIntervalSinceDate:start]);
	
	NSLog(@"Begin benchmark (KBCKit+KBKit)");
	start = [NSDate date];
	for (int i = 0; i < 200; i++) {
		NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
		NSData * encoded = kb_encode_full(benchmark);
		NSDictionary * decoded = (NSDictionary *)kb_decode_full(encoded);
		if (!decoded) {
			NSLog(@"Decode/encode of benchmark failed.");
			[pool drain];
			return;
		}
		keyedBitsSize = [encoded length];
		[pool drain];
	}
	NSLog(@"Benchmark complete: %lf", [[NSDate date] timeIntervalSinceDate:start]);
	
	NSLog(@"KeyedBits size: %ld", keyedBitsSize);
	NSLog(@"JSON size: %ld", jsonSize);
}
