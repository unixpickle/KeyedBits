package com.aqnichol.keyedbits.encode;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Map;

import com.aqnichol.keyedbits.value.ArrayValue;
import com.aqnichol.keyedbits.value.DataValue;
import com.aqnichol.keyedbits.value.DoubleValue;
import com.aqnichol.keyedbits.value.IntegerValue;
import com.aqnichol.keyedbits.value.MapValue;
import com.aqnichol.keyedbits.value.NullValue;
import com.aqnichol.keyedbits.value.StringValue;
import com.aqnichol.keyedbits.value.UnmatchedTypeException;
import com.aqnichol.keyedbits.value.Value;

public class ValueEncoder {

	private EncodeStreamWriter stream;
	
	public static byte[] encodeRootObject (Object object) {
		ByteEncodeStream stream = new ByteEncodeStream();
		EncodeStreamWriter writer = new EncodeStreamWriter(stream);
		ValueEncoder enc = new ValueEncoder(writer);
		enc.encodeObject(object);
		return stream.getBytes();
	}
	
	public static void encodeRootObjectToStream (Object object, OutputStream stream) throws IOException {
		try {
			FileEncodeStream encStream = new FileEncodeStream(stream);
			EncodeStreamWriter writer = new EncodeStreamWriter(encStream);
			ValueEncoder enc = new ValueEncoder(writer);
			enc.encodeObject(object);
		} catch (EncodeStreamWriteError e) {
			throw e.getOriginalException();
		}
	}
	
	public ValueEncoder (EncodeStreamWriter stream) {
		this.stream = stream;
	}
	
	public void encodeObject (Object anObject) {
		if (anObject == null) {
			encodeValue(new NullValue(null));
			return;
		}
		if (anObject instanceof Integer || anObject instanceof Long) {
			encodeValue(new IntegerValue(anObject));
		} else if (anObject instanceof Double || anObject instanceof Float) {
			encodeValue(new DoubleValue(anObject));
		} else if (anObject instanceof ArrayList) {
			encodeValue(new ArrayValue(anObject));
		} else if (anObject instanceof String) {
			encodeValue(new StringValue(anObject));
		} else if (anObject instanceof Map) {
			encodeValue(new MapValue(anObject));
		} else if (anObject instanceof ByteBuffer) {
			encodeValue(new DataValue(anObject));
		} else {
			throw new UnmatchedTypeException("Cannot encode objects of class: " + anObject.getClass().getName());
		}
	}
	
	public void encodeValue (Value value) {
		value.encodeToStream(stream);
	}
	
}
