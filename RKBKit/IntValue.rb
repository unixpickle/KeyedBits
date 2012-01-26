#!/usr/bin/ruby

require 'Value'

class Integer

	include KeyedBits::ValueEncode
	extend KeyedBits::ValueDecode

	def kbHeader()
		if (-214748647..2147483647).include?(self)
			KeyedBits::Header.new(KeyedBits::Header::TypeInteger, 2, true)
		else
			KeyedBits::Header.new(KeyedBits::Header::TypeInteger, 3, true)
		end
	end

	def kbWrite(stream)
		header = kbHeader
		stream.putc(header.encode)
		if header.lenLength == 2
			stream.write(KeyedBits::IntCoding.new(self).encode(4, true))
		else
			stream.write(KeyedBits::IntCoding.new(self).encode(8, true))
		end
	end

	def Integer.kbDecodeType()
		KeyedBits::Header::TypeInteger
	end

	def Integer.kbDecode(stream, header)
		intData = nil
		if header.lenLength == 2
			intData = stream.read(4)
			raise 'Reached EOF; expecting int(4) data' if !intData
			raise 'Invalid integer data length' if intData.length != 4
		elsif header.lenLength == 3
			intData = stream.read(8)
			raise 'Reached EOF; expecting int(8) data' if !intData
			raise 'Invalid integer data length' if intData.length != 8
		else raise 'Invalid length field length'
		end
		return KeyedBits::IntCoding.decode(intData, true).number
	end

end
