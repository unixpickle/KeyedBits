#!/usr/bin/ruby

require('Header')
require('IntCoding')

module KeyedBits

	def KeyedBits.readHeader(stream)
		byte = stream.getbyte
		return nil if !byte
		Header.decode(byte)
	end

	def KeyedBits.readDataValue(stream, header)
		if header.nullTerminated
			data = ""
			loop do
				nextByte = stream.getbyte
				raise 'Unexpected end in NULL termed value' if !nextByte
				break if nextByte == 0
				data << nextByte
			end
			data
		else
			lenBuff = stream.read(header.lenLength)
			if !lenBuff || lenBuff.length != header.lenLength
				raise 'Failed to read length field'
			end
			length = IntCoding.decode(lenBuff).number
			buffer = stream.read(length)
			if !buffer || buffer.length != length
				raise 'Failed to read full buffer'
			end
			buffer
		end
	end

end
