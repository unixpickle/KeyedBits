package com.aqnichol.keyedbits.value;

import com.aqnichol.keyedbits.decode.DecodeStreamReader;
import com.aqnichol.keyedbits.encode.EncodeStreamWriter;

public class NullValue extends Value {

	public NullValue (Object o) {
		super(o, Value.createType(KBValueTypeNull, true, (byte)1));
	}
	
	public NullValue (DecodeStreamReader stream, byte type) {
		super(stream, type);
	}
	
	protected void decodeFromStream (DecodeStreamReader stream) {
		// nothing to do
	}

	public void encodeToStream (EncodeStreamWriter stream) {
		stream.writeInt8(type);
	}
	
	public Object getObject () {
		return null;
	}

}
