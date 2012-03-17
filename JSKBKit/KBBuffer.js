KBBuffer = function (buffer) {
	if (typeof buffer == 'number') {
		this.buffer = buffer;
	} else if (buffer == undefined) {
		this.buffer = 512;
	}
	if (typeof buffer == "number" || buffer == undefined) {
		this.length = 0;
		this.array = new Uint8Array(this.buffer);
	} else {
		this.buffer = 0;
		this.offset = 0;
		this.array = buffer;
		this.length = buffer.length;
	}
}

KBBuffer.prototype.write = function (num) {
	if (this.buffer == 0) return null;

	if (typeof num == 'number') {
		// treat as byte
		if (this.length + 1 == this.array.length) {
			var arrCopy = new Uint8Array(this.array.length + this.buffer);
			for (var i = 0; i < this.length; i++) {
				arrCopy[i] = this.array[i];
			}
			this.array = arrCopy;
		}
		this.array[this.length] = num;
		this.length += 1;
	} else {
		// treat as array
		for (var i = 0; i < num.length; i++) {
			this.write(num[i]);
		}
	}
}

KBBuffer.prototype.write_uint = function (num, len) {
	var bytes = new Array(len);
	var numb = num;
	for (var i = 0; i < len; i++) {
		bytes[i] = numb & 255;
		numb >>= 8;
	}
	this.write(bytes);
}

/** READING **/

KBBuffer.prototype.read = function (size) {
	if (this.buffer != 0) return null;
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

