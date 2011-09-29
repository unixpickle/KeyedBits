KeyedBits from Java
===================

JKeyedBits is a lightweight set of classes that allows easy encoding and decoding of KeyedBits data from a Java program. It allows the encoding of many common classes including Map, ArrayList, and ByteBuffer.

Datatypes
==========

The KeyedBits format supports a number of datatypes. JKeyedBits choses to represent these types with several different classes:

 * **Strings:** The ```String``` class is used by JKeyedBits to represent strings. Any string can be encoded, as long as it only contains UTF-8 characters.
 * **Integers:** The ```Integer``` and ```Long``` class are used to represent integers. Although you can give the encoder either one, the decoder will only return a ```Long``` if the number cannot be represented by an ```Integer```.
 * **Doubles:** The ```Float``` and ```Double``` objects can be encoded. Although ```Float``` is supported for encoding, it will never be returned by any decode methods.
 * **Data:** Surprisingly, an array of bytes cannot be represented with the ```byte[]``` type. Instead, ```ByteBuffer``` objects must be used to encapsulate the array of bytes. When decoding raw binary data, the resulting object will always be an instance of ```ByteBuffer```.
 * **Arrays:** Arrays are represented by normal ```ArrayList<Object>``` objects. Arrays can only be encoded if they contain objects that JKeyedBits knows how to encode.
 * **Dictionaries:** Dictionaries, otherwise known as maps, are stored using typical ```Map<String, Object>``` objects. Any Map can be encoded, as long as its keys are ASCII strings, and it's values are limited to the classes supported by JKeyedBits. Every map that is decoded will result in an instance of ```HashMap```.
 * **Null:** If a null element is encountered in either an array or a dictionary, it will be encoded and decoded **as null**.

Examples
========

Encoding and decoding can be done using static methods on the ```ValueEncoder``` and ```ValueDecoder``` classes.  Here's a basic example of encoding and decoding a map:

    Map<String, Object> map;
    byte[] encoded = ValueEncoder.encodeRootObject(map);
    // decode the encoded byte array
    @SuppressWarnings("unchecked")
    Map<String, Object> decoded = (Map<String, Object>)ValueDecoder.decodeRootObject(encoded);
