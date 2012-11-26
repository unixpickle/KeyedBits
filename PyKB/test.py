import encodekb
import decodekb

stream = encodekb.KBDataEncodeStream()
enc = encodekb.KBEncoder(stream)
enc.write(['the quick brown fox', {'significance': None, 'data': bytearray('hello')}, 12345678900, 1234, 3.141592])

decStream = decodekb.KBDataDecodeStream(stream.data)
dec = decodekb.KBDecoder(decStream)
obj = dec.read()
print str(obj)

f = open('output', 'w')
f.write(stream.data)
f.close()
