class String

	# Adds support for the byteslice method prior to ruby v1.9
	unless method_defined?(:byteslice)
		def byteslice(*args)
			subElem = unpack('C*').slice(*args)
			if subElem.class.public_method_defined?(:pack)
				return subElem.pack('C*')
			else
				return subElem
			end
		end
	end

end
