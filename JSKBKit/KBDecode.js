KBDecode = function (buffer) {
	this.buffer = buffer;
}

KBDecode.prototype.decode = function () {
	var typeVal = this.buffer.read();
	if (typeVal == null) throw 'Failed to read byte';
	if (typeVal == 0) throw 'Invalid header type';

	var header = KBHeader.decode(typeVal);
	return this.decode_header(header);
}

KBDecode.prototype.decode_header = function (header) {
	if (header.type == KBHeader.TypeString) {
		return this.decode_string(header);
	} else if (header.type == KBHeader.TypeFloat) {
		return this.decode_float(header);
	} else if (header.type == KBHeader.TypeInteger) {
		return this.decode_integer(header);
	} else if (header.type == KBHeader.TypeArray) {
		return this.decode_array(header);
	} else if (header.type == KBHeader.TypeDictionary) {
		return this.decode_dictionary(header);
	} else if (header.type == KBHeader.TypeNull) {
		return this.decode_null(header);
	} else if (header.type == KBHeader.TypeData) {
		return this.decode_data(header);
	}
	return null;
}

KBDecode.prototype.decode_string = function (header) {
	var str = "";
	while (true) {
		var c = this.buffer.read();
		if (c == null) return null;
		if (c == 0) break;
		str = str + String.fromCharCode(c);
	}
	return str;
}

KBDecode.prototype.decode_float = function (header) {
	var str = "";
	while (true) {
		var c = this.buffer.read();
		if (c == null) return null;
		if (c == 0) break;
		str = str + String.fromCharCode(c);
	}
	return parseFloat(str);
}

KBDecode.prototype.decode_integer = function (header) {
	var len = Math.pow(2, header.length);
	var num = this.buffer.read_int(len);
	return num;
}

KBDecode.prototype.decode_null = function (header) {
	return null;
}

KBDecode.prototype.decode_data = function (header) {
	var len = this.buffer.read_uint(header.length);
	var buffer = new KBBuffer(len);
	buffer.write(this.buffer.read(len));
	return buffer;
}

KBDecode.prototype.decode_array = function (header) {
	var array = [];
	while (true) {
		var hd = this.buffer.read();
		var head = KBHeader.decode(hd);
		if (head.type == 0) break;
		obj = this.decode_header(head);
		array.push(obj);
	}
	return array;
}

KBDecode.prototype.decode_dictionary = function (header) {
	var dict = {};
	while (true) {
		var key = this.buffer.read_key();
		if (key == null) break;
		var obj = this.decode();
		dict[key] = obj;
	}
	return dict;
}

