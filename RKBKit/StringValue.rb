#!/usr/bin/ruby

require 'Value'

class String

	include KeyedBits::ValueEncode
	extend KeyedBits::ValueDecode

	def kbHeader()
		KeyedBits::Header.new(KeyedBits::Header::TypeString, 1, true)
	end

	def kbWrite(stream)
		header = kbHeader
		stream.putc(header.encode)
		each_byte do |byte|
			stream.putc(byte) if byte != 0
		end
		stream.putc(0)
	end

	def String.kbDecodeType()
		KeyedBits::Header::TypeString
	end

	def String.kbDecode(stream, header)
		KeyedBits.readDataValue(stream, header)
	end

end

