//
//  KBValueString.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/22/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBValueString.h"

NSString * _KBValueStringReadString (const char * buffer, NSUInteger * len);

@implementation KBValueString

- (id)initWithString:(NSString *)string {
	UInt8 theType = KBValueTypeFieldWithInfo(1, YES, KBValueTypeUTF8String);
	self = [super initWithValueType:theType object:string];
	return self;
}

- (id)initWithObject:(NSObject *)object {
	if (![object isKindOfClass:[NSString class]]) {
		[[super init] dealloc];
		return nil;
	}
	return (self = [self initWithString:(NSString *)object]);
}

- (id)initWithValueBytes:(const char *)buffer length:(NSUInteger *)lenInOut {
	if ((self = [super init])) {
		valueType = buffer[0];
		if (![self isNullTerminated] || [self lengthFieldLength] != 0) {
			[super dealloc];
			return nil;
		}
		valueObject = [_KBValueStringReadString(buffer, lenInOut) retain];
		if (!valueObject) {
			[super dealloc];
			return nil;
		}
	}
	return self;
}

- (NSString *)string {
	return (NSString *)valueObject;
}

- (BOOL)encodeToData:(NSMutableData *)theData {
	UInt8 nullByte = 0;
	NSData * encoded;
	
	[super encodeToData:theData];
	
	encoded = [[self string] dataUsingEncoding:NSUTF8StringEncoding allowLossyConversion:YES];
	[theData appendBytes:[encoded bytes] length:[encoded length]];
	[theData appendBytes:&nullByte length:1];
	
	return YES;
}

@end

NSString * _KBValueStringReadString (const char * buffer, NSUInteger * len) {
	NSMutableData * stringData = [[NSMutableData alloc] init];
	NSUInteger i;
	
	for (i = 1; i < *len; i++) {
		if (buffer[i] == 0) {
			break;
		}
		if (i + 1 == *len) {
			[stringData release];
			return nil;
		}
		[stringData appendBytes:&buffer[i] length:1];
	}
	
	*len = i + 1;
	
	NSString * string = [[NSString alloc] initWithData:stringData encoding:NSUTF8StringEncoding];
	[stringData release];
	return string;
}
