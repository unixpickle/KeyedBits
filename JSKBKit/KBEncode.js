KBEncode = function (buffer) {
	this.buffer = buffer;
}

KBEncode.prototype.encode = function (object) {
	if (typeof object == 'number') {
		if (Math.round(object) == object) {
			this.encode_int(object);
		} else {
			this.encode_float(object);
		}
	} else if (typeof object == 'string') {
		this.encode_string(object);
	} else if (typeof object == 'boolean') {
		this.encode_bool(object);
	} else if (object == undefined || object == null) {
		this.encode_null();
	} else if (typeof object == 'object') {
		if (object.constructor.name == 'Array') {
			this.encode_array(object);
		} else if (object.constructor.name == 'KBBuffer') {
			this.encode_buffer(object);
		} else {
			this.encode_dictionary(object);
		}
	} else {
		return false;
	}
	return true;
}

KBEncode.prototype.encode_null = function () {
	var header = new KBHeader(KBHeader.TypeNull, 1, false);
	this.buffer.write(header.encode());
	return true;
}

KBEncode.prototype.encode_bool = function (bool) {
	var num = (bool ? 1 : 0);
	return this.encode_int(num);
}

KBEncode.prototype.encode_string = function (string) {
	var header = new KBHeader(KBHeader.TypeString, 1, true);
	this.buffer.write(header.encode());
	this.buffer.write(string);
	this.buffer.write(0);
	return true;
}

KBEncode.prototype.encode_float = function (number) {
	var str = number.toString();
	var header = new KBHeader(KBHeader.TypeFloat, 1, true);
	this.buffer.write(header.encode());
	this.buffer.write(str);
	this.buffer.write(0);
	return true;
}

KBEncode.prototype.encode_int = function (number) {
	var header = null;
	var size = 4;
	if (number >= 2147483648 || number <= -2147483647) {
		header = new KBHeader(KBHeader.TypeInteger, 3, false);
		size = 8;
	} else {
		header = new KBHeader(KBHeader.TypeInteger, 2, false);
	}
	this.buffer.write(header.encode());
	this.buffer.write_int(number, size);
	return true;
}

KBEncode.prototype.encode_buffer = function (buffer) {
	var lenLength = 1;
	if (buffer.length >= Math.pow(2, 24)) {
		lenLength = 4;
	} else if (buffer.length >= Math.pow(2, 16)) {
		lenLength = 3;
	} else if (buffer.length >= 256) {
		lenLength = 2;
	}
	var header = new KBHeader(KBHeader.TypeData, lenLength, false);
	this.buffer.write(header.encode());
	this.buffer.write_uint(buffer.length, lenLength);
	this.buffer.write_buffer(buffer);
	return true;
}

KBEncode.prototype.encode_array = function (array) { 
	var header = new KBHeader(KBHeader.TypeArray, 1, true);
	this.buffer.write(header.encode());
	for (var i = 0; i < array.length; i++) {
		if (!this.encode(array[i])) return false;
	}
	this.buffer.write(0);
	return true;
}

KBEncode.prototype.encode_dictionary = function (dictObj) {
	var header = new KBHeader(KBHeader.TypeDictionary, 1, true);
	this.buffer.write(header.encode());
	for (var key in dictObj) {
		var obj = dictObj[key];
		this.buffer.write_key(key);
		if (!this.encode(obj)) return false;
	}
	this.buffer.write(0);
	return true;
}
