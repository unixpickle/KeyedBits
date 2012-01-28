KeyedBits from Ruby!
====================

RKBKit is an easy-to-use class library for encoding and decoding KeyedBits data with Ruby 1.8 and up.  It makes encoding basic Ruby datatypes such as String, Array, Hash, and Integer trivial, and also provides a `KeyedBits::BinaryString` class for representing binary data.  Anybody who plans on archiving basic data structures in their Ruby applications should consider KeyedBits over JSON, both for its data conservation and easy-of-use.

Example
=======

RKBKit allows you to encode KeyedBits data to any `IO` object, be that a file or a socket.  It would even be possible to create an `IO` subclass for raw binary data in memory, allowing for traditional to-and-from memory transcoding.  You can use the `kbWrite` method on any class that is supported by KeyedBits, passing an `IO` object to be used for output:

	file = File.new("output.kb", "w")
	[1, 2, "test", 3.14].kbWrite(file)
	file.close

Likewise, the `KeyedBits.kbRead` method takes an `IO`, and returns a decoded object:

	file = File.new("input.kb", "r")
	decoded = KeyedBits.kbRead(file)
	file.close

More examples can be found in `test.rb`.
