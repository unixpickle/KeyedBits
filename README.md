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

KBKit is my Objective-C implementation of KeyedBits. It can be found in the KBKit directory in the project root. If you run the KeyedBits Xcode project, you will see that I have written a number of tests, and included a benchmark. The benchmark compares KBKit to JSON-framework, encoding and decoding the same data 200 times each. Upon running the benchmark, you will find that JSON-framework takes about 50% of the time that KBKit does to run the benchmark. KBKit is designed to be elegant, using a very nice Object Oriented structure to encode and decode KeyedBits data. It is not designed for speed.

Upon running the benchmark, you will also see that the benchmark JSON (taken from the JSONKit github repository) encodes to 24,104 bytes of JSON. The same data represented as KeyedBits takes up only 21,094 bytes. This means that KeyedBits encodes data that is around 87% of the size of the equivalent JSON data.

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

Right now, speed is my #1 concern. Keyed bits is around twice as slow as JSON-framework, which is notorious for being slow to begin with. I am currently working on a C implementation of KeyedBits, but even that may not be able to beat some of the top-notch JSON encoders. Honestly, it would be great if anybody who has time would give me a hand in optimizing my code. I am willing to do whatever it takes to bump the speed of encoding/decoding to knock the competition out of the park.