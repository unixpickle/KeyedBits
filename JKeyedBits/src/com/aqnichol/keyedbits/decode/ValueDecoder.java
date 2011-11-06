package com.aqnichol.keyedbits.decode;

import java.io.IOException;
import java.io.InputStream;

import com.aqnichol.keyedbits.value.ArrayValue;
import com.aqnichol.keyedbits.value.DataValue;
import com.aqnichol.keyedbits.value.DoubleValue;
import com.aqnichol.keyedbits.value.IntegerValue;
import com.aqnichol.keyedbits.value.MapValue;
import com.aqnichol.keyedbits.value.NullValue;
import com.aqnichol.keyedbits.value.StringValue;
import com.aqnichol.keyedbits.value.UnmatchedTypeException;
import com.aqnichol.keyedbits.value.Value;

public class ValueDecoder {

	private DecodeStreamReader stream;
	
	public static Object decodeRootObject (byte[] data) {
		ByteDecodeStream stream = new ByteDecodeStream(data);
		DecodeStreamReader reader = new DecodeStreamReader(stream);
		ValueDecoder dec = new ValueDecoder(reader);
		Value value = dec.decodeNextValue();
		if (value == null) {
			throw new UnmatchedTypeException("Stream contained NULL terminator object.");
		}
		Object object = value.getObject();
		return object;
	}
	
	public static Object decodeRootObjectFromStream (InputStream stream) throws IOException {
		try {
			FileDecodeStream decStream = new FileDecodeStream(stream);
			DecodeStreamReader reader = new DecodeStreamReader(decStream);
			ValueDecoder dec = new ValueDecoder(reader);
			Value value = dec.decodeNextValue();
			if (value == null) {
				throw new UnmatchedTypeException("Stream contained NULL terminator object.");
			}
			Object object = value.getObject();
			return object;
		} catch (DecodeStreamReadError e) {
			throw e.getOriginalException();
		}
	}
	
	public ValueDecoder (DecodeStreamReader stream) {
		this.stream = stream;
	}
	
	public Value decodeNextValue () {
		byte type = stream.readUInt8();
		byte typeNumber = (byte)(type & 7);
		switch (typeNumber) {
			case Value.KBValueTypeTerminator:
				return null;
			case Value.KBValueTypeInteger:
				return new IntegerValue(stream, type);
			case Value.KBValueTypeFloating:
				return new DoubleValue(stream, type);
			case Value.KBValueTypeArray:
				return new ArrayValue(stream, type);
			case Value.KBValueTypeUTF8String:
				return new StringValue(stream, type);
			case Value.KBValueTypeData:
				return new DataValue(stream, type);
			case Value.KBValueTypeDictionary:
				return new MapValue(stream, type);
			case Value.KBValueTypeNull:
				return new NullValue(stream, type);
			default:
				throw new UnmatchedTypeException("Failed to find class matching type:" + (type & 0xff));
		}
	}
	
}
