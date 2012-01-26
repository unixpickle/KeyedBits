#!/usr/bin/ruby

require 'Value'
require 'Decoder'

class Array

	include KeyedBits::ValueEncode
	extend KeyedBits::ValueDecode

	def kbHeader()
		KeyedBits::Header.new(KeyedBits::Header::TypeArray, 1, true)
	end

	def kbWrite(stream)
		stream.putc(kbHeader.encode)
		each do |element|
			element.kbWrite(stream)
		end
		stream.putc(KeyedBits::Header::TypeTerminator)
	end

	def Array.kbDecodeType()
		KeyedBits::Header::TypeArray
	end

	def Array.kbDecode(stream, header)
		array = Array.new
		loop do
			object = KeyedBits.kbRead(stream)
			break if object == false
			array << object
		end
		array
	end

end

