#!/usr/bin/ruby

require 'KeyedBits'

class Hash

	def to_s
		str = "{"
		first = true
		each_key do |theKey|
			theValue = self[theKey]
			valStr = theValue.is_a?(String) ? "\"" + theValue + "\"" : theValue
			str << theKey + " = " + valStr.to_s
			str << ", " if first
			first = false
		end
		str << "}"
	end

end

file = File.new("testfile.kb", "w")
number = 2**32
string = "test123"
data = KeyedBits::BinaryString.new("\x13\x37\x06\x66\x09\x11")
float = 3.141592
hash = {'name' => 'alex', 'age' => 15}
[number, string, data, float, hash, nil].kbWrite(file)
file.close

file = File.new("testfile.kb", "r")
decoded = KeyedBits.kbRead(file)
file.close

decInt, decStr, decBin, decFlt, decHash, decNil = *decoded

puts "original: #{number} \"#{string}\" " + data.to_s + " #{float}"
puts "decoded: #{decInt} \"#{decStr}\" " + decBin.to_s + " #{decFlt}"
puts "original decoded as nil (success)" if decNil.nil?
puts "originalHash: " + hash.to_s
puts "decodedHash: " + decHash.to_s
