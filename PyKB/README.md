Usage
=====

To use PyKB to decode KeyedBits data which you have in an existing bytearray object, use the following:

    import decodekb
    ...
    decStream = decodekb.KBDataDecodeStream(myByteArray)
    obj = decodekb.KBDecoder(decStream).read()

PyKB will throw a KBTypeError when a decode operation fails.

You can encode a Python primitive such as a dictionary, array, integer, string, floating point, or bytearray object into KeyedBits data as follows:

    import encodekb
    ...
    encStream = encodekb.KBDataEncodeStream()
    encodekb.KBEncoder(encStream).write(myPrimitiveObject)
    theData = encStream.data

License
=======

Use this at your own whim under no warranty.
