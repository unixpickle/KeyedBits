#!/usr/bin/ruby

require 'Value'

class Float

	include KeyedBits::ValueEncode
	extend KeyedBits::ValueDecode

	def kbHeader()
		KeyedBits::Header.new(KeyedBits::Header::TypeFloat, 1, true)
	end

	def kbWrite(stream)
		header = kbHeader
		stream.putc(header.encode)
		to_s.each_byte do |byte|
			stream.putc(byte) if byte != 0
		end
		stream.putc(0)
	end

	def Numeric.kbDecodeType()
		KeyedBits::Header::TypeFloat
	end

	def Numeric.kbDecode(stream, header)
		KeyedBits.readDataValue(stream, header).to_f
	end

end

