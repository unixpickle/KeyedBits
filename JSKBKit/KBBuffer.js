KBBuffer = function (buffer) {
	if (typeof buffer == 'number') {
		this.buffer = buffer;
	} else if (buffer == undefined) {
		this.buffer = 512;
	}
	if (typeof buffer == "number" || buffer == undefined) {
		this.length = 0;
		this.offset = 0;
		this.array = new Uint8Array(this.buffer);
	} else if (typeof buffer == "string") {
		var charCodes = [];
		this.buffer = new Uint8Array(this.buffer.length);
		this.offset = 0;
		this.length = buffer.length;
		for (var i = 0; i < buffer.length; i++) {
			this.buffer[i] = buffer.charCodeAt(i);
		}
	} else {
		this.buffer = 0;
		this.offset = 0;
		this.array = buffer;
		this.length = buffer.length;
	}
}

KBBuffer.prototype.write = function (obj) {
	if (this.buffer <= 0) return false;

	if (typeof obj == 'number') {
		// treat as byte
		if (this.offset >= this.array.length) {
			var arrCopy = new Uint8Array(this.array.length + this.buffer);
			for (var i = 0; i < this.offset; i++) {
				arrCopy[i] = this.array[i];
			}
			this.array = arrCopy;
		}
		this.array[this.offset] = obj;
		this.offset++;
		if (this.offset > this.length) {
			this.length = this.offset;
		}
	} else {
		// treat as array
		for (var i = 0; i < obj.length; i++) {
			if (!this.write(obj[i])) return false;
		}
	}
	return true;
}

KBBuffer.prototype.write_uint = function (num, len) {
	var bytes = new Array(len);
	var numb = num;
	for (var i = 0; i < len; i++) {
		bytes[i] = numb & 255;
		numb >>= 8;
	}
	return this.write(bytes);
}

KBBuffer.prototype.write_int = function (num, len) {
	var bytes = new Array(len);
	var numb = Math.abs(num);
	for (var i = 0; i < len; i++) {
		bytes[i] = numb & 255;
		numb >>= 8;
	}
	if (num < 0) {
		// two's compliment
		var add = 1;
		for (var i = 0; i < len; i++) {
			bytes[i] ^= 255;
			bytes[i] += add;
			if (bytes[i] > 255) {
				bytes[i] = 0;
			} else {
				add = 0;
			}
		}
	}
	return this.write(bytes);
}

/** READING **/

KBBuffer.prototype.read = function (size) {
	if (typeof size != 'number') {
		if (this.offset < this.length) {
			var v = this.array[this.offset];
			this.offset++;
			return v;
		}
	} else {
		var buff = [];
		for (var i = 0; i < size; i++) {
			var c = this.read();
			if (c == null) return null;
			buff.push(c);
		}
		return buff;
	}
	return null;
}

KBBuffer.prototype.read_uint = function (size) {
	var bytes = this.read(size);
	if (bytes == null) return null;
	var number = 0;
	var shr = 0;
	for (var i = 0; i < bytes.length; i++) {
		number |= bytes[i] << shr;
		shr += 8;
	}
	return number;
}

KBBuffer.prototype.read_int = function (size) {
	var bytes = this.read(size);
	if (bytes == null) return null;
	
	var sign = 1;
	if ((bytes[size - 1] & 128) == 128) {
		// signed negative
		sign = -1;
		var sub = 1;
		for (var i = 0; i < bytes.length; i++) {
			bytes[i] -= sub;
			if (bytes[i] == -1) {
				bytes[i] = 255;
			} else {
				sub = 0;
			}
			bytes[i] ^= 255;
		}
	}
	
	var number = 0;
	var shr = 0;
	for (var i = 0; i < bytes.length; i++) {
		number |= bytes[i] << shr;
		shr += 8;
	}
	return number * sign;
}
