/** This is how encoding/decoding might look **/

require('./node_include.js');

// encode
var enc = new KBEncode(new KBBuffer(1));
enc.encode([1337, -3.14, -65536, 'test', false, null, {'hi': 10}]);

// get encoded buffer for decoding
var buff = enc.buffer;
buff.offset = 0;

// decode
var dec = new KBDecode(buff);
var decoded = dec.decode();
console.log(decoded);
