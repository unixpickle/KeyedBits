KBHeader = function (type, length, nullTerm) {
	this.type = type;
	this.length = length;
	this.nullTerm = nullTerm;
}

KBHeader.prototype.encode = function () {
	var encoded = 0;
	if (this.nullTerm) encoded |= 128
	encoded |= (this.length - 1) << 5;
	encoded |= this.type;
	return encoded;
}

KBHeader.decode = function (encoded) {
	var valType = encoded & 15;
	var length = ((encoded >> 5) & 3) + 1;
	var nullTerm = false;
	if ((encoded >> 7) & 1 == 1) nullTerm = true;
	return new KBHeader(valType, length, nullTerm);
}

KBHeader.TypeTerminator = 0;
KBHeader.TypeString = 1;
KBHeader.TypeArray = 2;
KBHeader.TypeDictionary = 3;
KBHeader.TypeNull = 4;
KBHeader.TypeData = 5;
KBHeader.TypeInteger = 6;
KBHeader.TypeFloat = 7;

