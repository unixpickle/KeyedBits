package com.aqnichol.keyedbits.value;

import com.aqnichol.keyedbits.decode.DecodeStreamReader;
import com.aqnichol.keyedbits.encode.EncodeStreamWriter;

public abstract class Value {
	
	public static final byte KBValueTypeTerminator = 0;
	public static final byte KBValueTypeUTF8String = 1;
	public static final byte KBValueTypeArray = 2;
	public static final byte KBValueTypeDictionary = 3;
	public static final byte KBValueTypeNull = 4;
	public static final byte KBValueTypeData = 5;
	public static final byte KBValueTypeInteger = 6;
	public static final byte KBValueTypeFloating = 7;

	protected byte type;
	protected Object object;
	
	public static byte createType (byte type, boolean nullTerm, byte lenLen) {
		byte number = 0;
		number |= (type & 7);
		if (nullTerm) {
			number |= 128;
		}
		number |= ((lenLen - 1) & 3) << 5;
		return number;
	}
	
	protected Value (Object object, byte type) {
		this.object = object;
		this.type = type;
	}
	
	public Value (Object object) {
		this.type = 0;
		this.object = object;
	}
	
	public Value (DecodeStreamReader stream, byte type) {
		this.type = type;
	}
	
	protected abstract void decodeFromStream (DecodeStreamReader stream);
	public abstract void encodeToStream (EncodeStreamWriter stream);
	
	// These methods to not need to be implemented in a subclass.
	
	public int getType () {
		return type;
	}
	
	public int getTypeNumber () {
		return (int)(type & 7);
	}
	
	public boolean getTypeIsNullTerminated () {
		if ((type & 128) == 0) {
			return false;
		} else {
			return true;
		}
	}
	
	public int getTypeLengthFieldLength () {
		int number = (int)((type >> 5) & 3);
		return number + 1;
	}
	
	public Object getObject () {
		return object;
	}
	
}
