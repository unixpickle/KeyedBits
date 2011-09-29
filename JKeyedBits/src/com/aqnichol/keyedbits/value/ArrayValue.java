package com.aqnichol.keyedbits.value;

import java.util.ArrayList;

import com.aqnichol.keyedbits.decode.DecodeStreamReader;
import com.aqnichol.keyedbits.decode.ValueDecoder;
import com.aqnichol.keyedbits.encode.EncodeStreamWriter;
import com.aqnichol.keyedbits.encode.ValueEncoder;

public class ArrayValue extends Value {
	
	public ArrayValue (DecodeStreamReader stream, byte type) {
		super(stream, type);
		decodeFromStream(stream);
	}
	
	public ArrayValue (Object anObject) {
		super(anObject);
		if (!(anObject instanceof ArrayList)) {
			throw new UnmatchedTypeException("Invalid class for array: " + anObject.getClass().getName());
		}
		object = anObject;
		type = Value.createType(KBValueTypeArray, true, (byte)1);
	}
	
	@SuppressWarnings("unchecked")
	public ArrayList<Object> getArrayList () {
		return (ArrayList<Object>)getObject();
	}
	
	public void encodeToStream (EncodeStreamWriter stream) {
		stream.writeInt8(type);
		ValueEncoder enc = new ValueEncoder(stream);
		ArrayList<Object> objects = getArrayList();
		for (int i = 0; i < objects.size(); i++) {
			Object obj = objects.get(i);
			enc.encodeObject(obj);
		}
		// NULL termination
		stream.writeInt8((byte)0);
	}
	
	protected void decodeFromStream (DecodeStreamReader stream) {
		ArrayList<Object> list = new ArrayList<Object>();
		ValueDecoder dec = new ValueDecoder(stream);
		Value value;
		while ((value = dec.decodeNextValue()) != null) {
			list.add(value.getObject());
		}
		object = list;
	}
	
}
