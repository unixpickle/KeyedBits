#!/usr/bin/ruby

module KeyedBits

	def KeyedBits.bytesRequiredForLength(aLength)
		case aLength
			when (0..255) then 1
			when (256..65535) then 2
			when (65536..16777215) then 3
			else 4
		end
	end

	class Header
		TypeTerminator = 0
		TypeString = 1
		TypeArray = 2
		TypeDictionary = 3
		TypeNull = 4
		TypeData = 5
		TypeInteger = 6
		TypeFloat = 7
		attr_reader :valueType, :lenLength, :nullTerminated
	
		def initialize(valueType, length, nullTermed)
			@valueType = valueType
			@lenLength = length
			@nullTerminated = nullTermed
		end

		def Header.decode(byte)
			valueType = byte & 0xf
			lenLength = ((byte >> 5) & 3) + 1
			nullTerminated = ((byte >> 7) & 1 == 1)
			Header.new(valueType, lenLength, nullTerminated)
		end
		
		def encode()
			raise 'Invalid lenLength' if !(0..3).include?@lenLength
			result = 0
			result |= 128 if nullTerminated
			result |= (lenLength - 1) << 5
			result |= valueType
			return result
		end
	end

end
