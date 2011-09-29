package com.aqnichol.keyedbits.value;

import com.aqnichol.keyedbits.decode.DecodeStreamReader;
import com.aqnichol.keyedbits.encode.EncodeStreamWriter;

public class DoubleValue extends Value {

	public DoubleValue (DecodeStreamReader stream, byte type) {
		super(stream, type);
		decodeFromStream(stream);
	}
	
	public DoubleValue (Object object) {
		super(object);
		if (object instanceof Float) {
			// convert it to a Double object
			this.object = new Double(((Float)object).doubleValue());
		} else if (!(object instanceof Double)) {
			throw new UnmatchedTypeException("Invalid double class:" + object.getClass().getName());
		}
		type = Value.createType(KBValueTypeFloating, true, (byte)1);
	}
	
	protected void decodeFromStream (DecodeStreamReader stream) {
		if (this.getTypeNumber() != Value.KBValueTypeFloating) {
			throw new UnmatchedTypeException("Invalid type field: " + this.getTypeNumber());
		}
		if (!this.getTypeIsNullTerminated()) {
			throw new UnmatchedTypeException("Type field should always indicate NULL terminated doubles."); 
		}
		
		String doubleString = stream.readNullTerminatedString();
		object = new Double(doubleString);
	}
	
	public void encodeToStream (EncodeStreamWriter stream) {
		stream.writeInt8(type);
		stream.writeNullTerminated(((Double)object).toString());
	}
	
	public double getDoubleValue () {
		return ((Double)object).doubleValue();
	}

}
