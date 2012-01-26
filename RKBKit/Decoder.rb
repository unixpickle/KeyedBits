#!/usr/bin/ruby

module KeyedBits

	def KeyedBits.kbRead(stream)
		classes = [Integer, String, BinaryString, NilClass,
				   Numeric, Array, Hash]
		header = KeyedBits.readHeader(stream)
		raise 'Reached EOF; expecting header' if !header
		return false if header.valueType == Header::TypeTerminator
		classes.each do |aClass|
			if aClass.kbDecodeType == header.valueType
				return aClass.kbDecode(stream, header)
			end
		end
		raise 'No class found for encoded data.'
	end

end
