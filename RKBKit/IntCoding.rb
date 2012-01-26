#!/usr/bin/ruby

module KeyedBits

	class IntCoding

		attr_reader :number

		def IntCoding.bigEndian()
			"\x01\x00".unpack("S")[0] != 1
		end

		public

		def initialize(number)
			@number = number
		end
	
		def IntCoding.decode(bytes, signed=false)
			# assuming little endian
			nativeBytes = (IntCoding.bigEndian ? bytes.reverse : bytes)
			num = case bytes.bytesize
				when 1 then bytes.unpack(signed ? 'c' : 'C')
				when 2 then nativeBytes.unpack(signed ? 's' : 'S')
				when 3
					if IntCoding.bigEndian
						nativeBytes = "\x00" + nativeBytes 
					else
						nativeBytes << "\x00"
					end
					nativeBytes.unpack(signed ? 'l' : 'L')
				when 4 then nativeBytes.unpack(signed ? 'l' : 'L')
				when 8 then nativeBytes.unpack(signed ? 'q' : 'Q')
				else raise 'Invalid length of decoding integer'
			end
			IntCoding.new(num[0])
		end

		def encode(length=4, signed=false)
			nativeEncoded = nil
			case length
				when 1 then nativeEncoded = [@number].pack(signed ? 'c' : 'C')
				when 2 then nativeEncoded = [@number].pack(signed ? 's' : 'S')
				when 3
					nativeEncoded = [@number].pack(signed ? 'l' : 'L')
					# slice off most significant byte
					if IntCoding.bigEndian
						nativeEncoded = nativeEncoded.byteslice(1, 3)
					else
						nativeEncoded = nativeEncoded.byteslice(0, 3)
					end
				when 4 then nativeEncoded = [@number].pack(signed ? 'l' : 'L')
				when 8 then nativeEncoded = [@number].pack(signed ? 'q' : 'Q')
				else raise 'Invalid length for encoding an integer'
			end
			nativeEncoded.reverse! if IntCoding.bigEndian
			nativeEncoded
		end

	end

end
