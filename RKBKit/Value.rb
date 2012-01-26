#!/usr/bin/ruby

require 'Reader'

module KeyedBits
	# should be used with "include"
	module ValueEncode
		def kbHeader()
			raise 'Must override kbHeader'
		end
		def kbWrite(stream)
			raise 'Must override kbWrite'
		end
	end

	# should be used with "extend"
	module ValueDecode
		def kbDecodeType()
			raise 'Must override kbDecodeType'
		end
		def kbDecode(stream, header)
			raise 'Must override kbDecode'
		end
	end
end
