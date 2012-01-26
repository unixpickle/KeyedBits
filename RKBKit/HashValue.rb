#!/usr/bin/ruby

require 'Value'
require 'Decoder'

class Hash

	include KeyedBits::ValueEncode
	extend KeyedBits::ValueDecode

	def kbHeader()
		KeyedBits::Header.new(KeyedBits::Header::TypeDictionary, 1, true)
	end

	def kbWrite(stream)
		stream.putc(kbHeader.encode)
		each_key do |aKey|
			Hash.writeKey(stream, aKey)
			self[aKey].kbWrite(stream)
		end
		stream.putc(0)
	end

	def Hash.kbDecodeType()
		KeyedBits::Header::TypeDictionary
	end

	def Hash.kbDecode(stream, header)
		hash = Hash.new
		loop do
			key = Hash.readKey(stream)
			break if key.nil?
			value = KeyedBits.kbRead(stream)
			hash[key] = value
		end
		hash
	end

	private

	def Hash.writeKey(stream, keyName)
		i = 0
		keyName.each_byte do |byte|
			cleanByte = (byte & 128) != 0 ? byte ^ 128 : byte
			cleanByte |= 128 if i == keyName.bytesize - 1
			stream.putc(cleanByte)
			i = i + 1
		end
	end

	def Hash.readKey(stream)
		str = ""
		loop do
			aChar = stream.getbyte
			raise 'Unexpected EOF reading hash key' if !aChar
			return nil if aChar == 0 && str.length == 0
			clear = (aChar & 128) != 0 ? aChar ^ 128 : aChar
			str << clear
			break if aChar != clear
		end
		str
	end

end

