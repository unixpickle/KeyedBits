package com.aqnichol.keyedbits.value;

import com.aqnichol.keyedbits.decode.DecodeStreamReader;
import com.aqnichol.keyedbits.encode.EncodeStreamWriter;

public class StringValue extends Value {

	public StringValue (Object object) {
		super(object);
		if (!(object instanceof String)) {
			throw new UnmatchedTypeException("Invalid class for string:" + object.getClass().getName());
		}
		type = Value.createType(KBValueTypeUTF8String, true, (byte)1);
	}

	public StringValue (DecodeStreamReader stream, byte type) {
		super(stream, type);
		// read NULL terminated string
		decodeFromStream(stream);
	}

	protected void decodeFromStream (DecodeStreamReader stream) {
		object = stream.readNullTerminatedString();
	}

	public void encodeToStream (EncodeStreamWriter stream) {
		stream.writeInt8(type);
		stream.writeNullTerminated((String)object);
	}

}
