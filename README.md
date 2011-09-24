Binary Archiving
================

Most of you are probably at least somewhat familiar with the Property List (plist) format. You can encode an NSDictionary or an NSArray as a plist using Cocoa. What you may not have known is that plist supports both an XML and a binary file format, both of which can accurately represent all primitive data types. Unfortunately, both formats also take up quite a bit more storage than they need to. This is because Apple's binary plist stores tons of un-needed metadata and other information. Therefore, my goal was to make a *better* binary dictionary archiver.

Need for Conservation
=====================

I designed the KeyedBits format with space conservation in mind. I was writing this format to be a major binary competitor with JSON, a human-readable object notation. JSON has quite a low overhead, usually needing around two bytes to denote an empty object, whether it be a string, array, or dictionary. The only way for me to beat this was to one up them, and make every empty object take only a single byte of overhead.

KeyedBits uses NULL termination heavily. All C programmers know and love the idea of NULL termination; simply having a NULL element at the end of the array. The elegance of NULL termination removes the need for a length field, which saves quite a bit of space when storing strings longer than 64k. Since every object has a single-byte type field preceding the data, it was natural for me to decide that a type field of **0x0** indicated a NULL object. This allows NULL termination to apply to arrays and dictionaries, which are terminated by a NULL object.

Now seems like a good time to mention that, despite its name, the object type field holds both the type of the object, as well as the termination method. The termination method field indicates whether or not the object is NULL terminated, and, if not, the length of the following *length* field. The *length length* field is two bits, and is one lower than the actual length of the length field. Therefore, 0b11 would be the *length length* field for a data object that needs 4 bytes to represent its length.

Integers use the *length length* field in a decisive way. Since integers are always either 4 or 8 bytes, it seemed natural to use the *length length* field to indicate the length of the integer itself. The size of the integer (in bytes) can be found by adding one to the *length length* field, and then raising 2 to that power. Therefore, if the *length length* field is 2, the length of the integer is 2^3 bytes, or 8.

KBKit
=====

KBKit is my Objective-C implementation of KeyedBits. It can be found in the KBKit directory in the project root. KBCKit uses the Object Oriented programming model to its advantage, with a base value class called *KBValue*. This class is then subclassed by different classes designed for different data types.

KBCKit
======

*KBCKit* is a C framework, lightly wrapped with Objective-C, that is designed to be faster than KBKit. Currently, *KBCKit* only has encoding capabilities, which are around 2x faster than *KBKit*'s encoder, and 140% of JSON-framework's encoding speed. Soon I believe that *KBCKit* will also have decoding abilities, which will render *KBKit* obsolete. One might argue that *KBKit* is more elegant, but personally I believe that the architecture of *KBCKit* is actually much cleaner and light-weight.

Benchmark
=========

If you run the KeyedBits Xcode project, you will see that I have written a number of tests, and included a benchmark. The benchmark compares KBKit+KBCKit to JSON-framework, encoding and decoding the same data 200 times each. Upon running the benchmark, you will find that JSON-framework takes about 50% of the time that KBKit+KBCKit does to run the benchmark. KBKit is designed to be elegant, using a very nice Object Oriented structure to encode and decode KeyedBits data. It is not designed for speed.

Upon running the benchmark, you will see that the benchmark dictionary (taken from the JSONKit github repository) encodes to 24,104 bytes of JSON. The same data represented as KeyedBits binary takes up only 21,094 bytes. This means that KeyedBits encodes data that is around 87% of the size of the equivalent JSON data. You would also find that, when encoding large pieces of binary data into KeyedBits, that KeyedBits may save up to 50% over using base64 with JSON!

Advantages of KeyedBits
=======================

**Space conservation**
KeyedBits takes heavy advantage of NULL termination, and when needed, length fields. This allows it to efficiently represent data with only one or two bytes of overhead, which is usually a byte or two less than that of JSON. If you are working with large dictionaries or arrays, you will save a noticeable amount of space that you would not when using XML, JSON, or even binary plist.

**Data encoding**
Besides storage conservation, you will notice that KeyedBits allows for the encoding of raw data. This feature comes with the binary data method of archiving, and cannot be achieved in JSON without base64 or likewise. This being said, KeyedBits is a great alternative for network protocols or file formats that need to store image data, or any other form of binary.

**Lossless transformation from object to data, then back to object again**
The KBKit framework provides a one-line interface for encoding any compatible NSObject to and from KeyedBits ```NSData``` objects. Double's do not lose their precision, strings to not lose their UTF8 characters, and data loses absolutely nothing. This makes KeyedBits ideal for any sort of file format or network protocol where dictionaries need to be represented as data.

Disadvantages of KeyedBits
==========================

**Lack of support and popularity**
If you search the internet for *KeyedBits*, you will find *Smith Optics' ingenious Pivlock system*. You will not find a single library other than KBKit for encoding and decoding KeyedBits data, nor will you find a specification or documentation. I am currently the only developer of whom I am aware that contributes to the KeyedBits project. That being said, it would be nice if someone would take the time to write an implementation of KeyedBits for another language. The more cross-platform KeyedBits becomes, the more widely used it will be able to be.

**Speed**
Right now, speed is my #1 concern. *KBKit* is around twice as slow as JSON-framework, which is notorious for being slow to begin with. I am currently working on a C implementation (KBCKit) of KeyedBits, but even that may not be able to beat some of the top-notch JSON encoders. Honestly, it would be great if anybody who has time would give me a hand in optimizing my code. I am willing to do whatever it takes to bump the speed of encoding/decoding to knock the competition out of the park.

License
=======

	Copyright (c) 2011, Alex Nichol
	All rights reserved.

	Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

	Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
	Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.