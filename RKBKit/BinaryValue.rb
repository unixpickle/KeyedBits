#!/usr/bin/ruby
	
require 'StringValue'

module KeyedBits
	
	class BinaryString < String
	
		def kbHeader()
			lenLen = KeyedBits.bytesRequiredForLength(self.bytesize)
			Header.new(Header::TypeData, lenLen, false)
		end
	
		def kbWrite(stream)
			header = kbHeader
			stream.putc(header.encode)
			# write length + data
			len = header.lenLength
			encodedLen = IntCoding.new(self.bytesize).encode(len)
			stream.write(encodedLen)
			stream.write(self)
		end
	
		def BinaryString.kbDecodeType()
			Header::TypeData
		end
	
		def BinaryString.kbDecode(stream, header)
			BinaryString.new(KeyedBits.readDataValue(stream, header))
		end
	
		def to_s
			buff = "<"
			bytes = unpack('C' * length).collect {|x| x.to_s(16)}
			for i in (0..(bytes.length - 1))
				if i % 4 == 0 && i > 0
					buff << " "
				end
				buff << bytes[i].rjust(2, '00')
			end
			buff << ">"
		end
	
	end

end
