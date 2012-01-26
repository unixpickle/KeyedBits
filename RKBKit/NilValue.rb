#!/usr/bin/ruby

require 'Value'

class NilClass

	include KeyedBits::ValueEncode
	extend KeyedBits::ValueDecode

	def kbHeader()
		KeyedBits::Header.new(KeyedBits::Header::TypeNull, 1, false)
	end

	def kbWrite(stream)
		stream.putc(kbHeader.encode)
	end

	def NilClass.kbDecodeType()
		KeyedBits::Header::TypeNull
	end

	def NilClass.kbDecode(stream, header)
		nil
	end

end

