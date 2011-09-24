//
//  KBValueDictionary.m
//  KeyedBits
//
//  Created by Alex Nichol on 9/23/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "KBValueDictionary.h"
#import "KBObjectCoder.h"

void _ANTermStringWrite (NSString * ascii, NSMutableData * data);
NSInteger _ANTermStringRead (NSMutableString * string, const char * bytes, NSUInteger length);

@implementation KBValueDictionary

- (id)initWithDictionary:(NSDictionary *)dictionary {
	UInt8 type = KBValueTypeFieldWithInfo(1, YES, KBValueTypeDictionary);
	return (self = [super initWithValueType:type object:dictionary]);
}

- (id)initWithObject:(NSObject *)object {
	if (![object isKindOfClass:[NSDictionary class]]) {
		[super dealloc];
		return nil;
	}
	return (self = [self initWithDictionary:(NSDictionary *)object]);
}

- (id)initWithValueBytes:(const char *)buffer length:(NSUInteger *)lenInOut {
	if ((self = [super init])) {
		if (*lenInOut < 2) {
			[super dealloc];
			return nil;
		}
		valueType = buffer[0];
		if (![self isNullTerminated]) {
			[super dealloc];
			return nil;
		}
		
		NSMutableDictionary * dictionary = [[NSMutableDictionary alloc] init];
		valueObject = dictionary;
		
		NSUInteger index = 1;
		while (index < *lenInOut) {
			if (buffer[index] == 0) {
				index++;
				break;
			}
			NSMutableString * string = [[NSMutableString alloc] init];
			Class decodeClass;
			KBValue * value;
			NSUInteger bytesLeft;
			NSInteger length = _ANTermStringRead(string, &buffer[index], *lenInOut - index);
			if (length < 0) {
				[string release];
				[super dealloc];
				return nil;
			}
			
			index += length;
			bytesLeft = *lenInOut - index;
			
			UInt8 classNumber = buffer[index];
			decodeClass = [KBObjectCoder valueClassForType:classNumber];
			if (decodeClass == Nil) {
				[string release];
				[super dealloc];
				return nil;
			}
			
			// decode with the current bytes
			value = [(KBValue *)[decodeClass alloc] initWithValueBytes:&buffer[index] length:&bytesLeft];
			if (!value) {
				[string release];
				[super dealloc];
				return nil;
			}
			
			index += bytesLeft;
			
			[dictionary setObject:[value valueObject] forKey:string];
			[string release];
			[value release];
		}
		
		*lenInOut = index;
	}
	return self;
}

- (NSDictionary *)dictionary {
	return (NSDictionary *)valueObject;
}

- (BOOL)encodeToData:(NSMutableData *)theData {
	UInt8 terminator = 0;
	[super encodeToData:theData];
	NSArray * keys = [[self dictionary] allKeys];
	for (int i = 0; i < [keys count]; i++) {
		NSString * key = [keys objectAtIndex:i];
		if (![key isKindOfClass:[NSString class]]) {
			return NO;
		}
		_ANTermStringWrite(key, theData);
		NSObject * valueObj = [[self dictionary] objectForKey:key];
		Class valueClass = [KBObjectCoder valueClassForObject:valueObj];
		if (valueObject == Nil) {
			return NO;
		}
		KBValue * value = [(KBValue *)[valueClass alloc] initWithObject:valueObj];
		if (!value) {
			return NO;
		}
		[value encodeToData:theData];
		[value release];
	}
	[theData appendBytes:&terminator length:1];
	return YES;
}

@end

void _ANTermStringWrite (NSString * ascii, NSMutableData * data) {
	UInt8 termFlag = 0x80;
	NSData * asciiData = [ascii dataUsingEncoding:NSASCIIStringEncoding allowLossyConversion:YES];
	if (!asciiData || [asciiData length] == 0) {
		[data appendBytes:&termFlag length:1];
		return;
	}
	for (int i = 0; i < [asciiData length]; i++) {
		UInt8 token = ((const unsigned char *)[asciiData bytes])[i];
		if ((token & termFlag) == 0) {
			if (i + 1 == [asciiData length]) {
				token |= termFlag;
			}
			[data appendBytes:&token length:1];
		} else {
			NSLog(@"Warning: ASCII string was not truly ASCII.");
			[data appendBytes:&termFlag length:1];
			return;
		}
	}
}

NSInteger _ANTermStringRead (NSMutableString * string, const char * bytes, NSUInteger length) {
	if (length == 0) return -1;
	UInt8 termFlag = 0x80;
	NSUInteger i;
	for (i = 0; i < length; i++) {
		UInt8 token = bytes[i];
		unsigned char appendChar = 0;
		if ((token & termFlag) != 0) {
			appendChar = token ^ termFlag;
		} else {
			appendChar = token;
		}
		
		if (appendChar == 0) {
			break;
		}
		[string appendFormat:@"%c", appendChar];
		if ((token & termFlag) != 0) {
			break;
		}
		if (i + 1 == length) return -1;
	}
	return i + 1;
}
