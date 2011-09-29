package com.aqnichol.keyedbits.value;

import com.aqnichol.keyedbits.decode.DecodeStreamReader;
import com.aqnichol.keyedbits.encode.EncodeStreamWriter;

public class IntegerValue extends Value {
	
	public IntegerValue (DecodeStreamReader stream, byte type) {
		super(stream, type);
		decodeFromStream(stream);
	}
	
	public IntegerValue (Object object) {
		super(object);
		
		long number = 0;
		if (object instanceof Integer) {
			number = ((Integer)object).intValue();
		} else if (object instanceof Long) {
			number = ((Long)object).longValue();
		} else {
			throw new UnmatchedTypeException("Invalid integer class: " + object.getClass().getName());
		}
		
		if (number < Integer.MIN_VALUE || number > Integer.MAX_VALUE) {
			type = Value.createType(KBValueTypeInteger, false, (byte)3);
			object = new Long(number);
		} else {
			type = Value.createType(KBValueTypeInteger, false, (byte) 2);
			object = new Integer((int)number);
		}
	}
	
	protected void decodeFromStream (DecodeStreamReader stream) {
		if (this.getTypeNumber() != Value.KBValueTypeInteger) {
			throw new UnmatchedTypeException("Invalid type field: " + this.getTypeNumber());
		}
		
		// use length field to determine size of integer
		if (this.getTypeLengthFieldLength() == 2) {
			object = new Integer(stream.readInt32());
		} else if (this.getTypeLengthFieldLength() == 3) {
			object = new Long(stream.readInt64());
		} else {
			throw new UnmatchedTypeException("Invalid length length value: " + this.getTypeLengthFieldLength() 
					+ ", type: " + this.getType());
		}
	}
	
	public long getNumberValue () {
		if (object instanceof Long) {
			return ((Long)object).longValue();
		} else if (object instanceof Integer) {
			return ((Integer)object).intValue();
		}
		return 0;
	}
	
	public void encodeToStream (EncodeStreamWriter stream) {
		stream.writeInt8(type);
		if (this.getTypeLengthFieldLength() == 3) {
			long number = getNumberValue();
			stream.writeInt64(number);
		} else if (this.getTypeLengthFieldLength() == 2) {
			int number = (int)getNumberValue();
			stream.writeInt32(number);
		}
	}
	
}
