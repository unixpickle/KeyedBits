package com.aqnichol.keyedbits.value;

import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.aqnichol.keyedbits.decode.DecodeStreamReader;
import com.aqnichol.keyedbits.decode.ValueDecoder;
import com.aqnichol.keyedbits.encode.EncodeStreamWriter;
import com.aqnichol.keyedbits.encode.ValueEncoder;

public class MapValue extends Value {

	public MapValue (Object object) {
		super(object, Value.createType(KBValueTypeDictionary, true, (byte)1));
		if (!(object instanceof Map)) {
			throw new UnmatchedTypeException("Invalid map class: " + object.getClass().getName());
		}
	}
	
	public MapValue (DecodeStreamReader stream, byte type) {
		super(stream, type);
		decodeFromStream(stream);
	}
	
	@SuppressWarnings("unchecked")
	public Map<String, Object> getMap () {
		return (Map<String, Object>)object;
	}
	
	protected void decodeFromStream (DecodeStreamReader stream) {
		// read keys + objects until the end
		String key = null;
		HashMap<String, Object> map = new HashMap<String, Object>();
		ValueDecoder decoder = new ValueDecoder(stream);
		while ((key = readMapKey(stream)) != null) {
			Value value = decoder.decodeNextValue();
			map.put(key, value.getObject());
		}
		object = map;
	}

	public void encodeToStream (EncodeStreamWriter stream) {
		stream.writeInt8(type);
		Map<String, Object> map = getMap();
		Set<String> entries = map.keySet();
		ValueEncoder encoder = new ValueEncoder(stream);
		for (String key : entries) {
			Object value = map.get(key);
			writeMapKey(stream, key);
			encoder.encodeObject(value);
		}
		stream.writeInt8((byte)0);
	}
	
	private static String readMapKey (DecodeStreamReader stream) {
		StringBuilder builder = new StringBuilder();
		byte b;
		
		do {
			b = stream.readUInt8();
			int cint = (int)(b & 0xff);
			if (cint == 0) {
				return null;
			}
			if ((cint & 0x80) != 0) {
				cint ^= 0x80;
			}
			if (cint != 0) {
				builder.append((char)cint);
			} else {
				break;
			}
		} while ((b & 0x80) == 0);
				
		return builder.toString();
	}
	
	private static void writeMapKey (EncodeStreamWriter stream, String keyString) {
		byte[] bytes = keyString.getBytes(Charset.forName("US-ASCII"));
		if (bytes == null) {
			throw new UnmatchedTypeException("The key \"" + keyString + "\" is not ASCII");
		}
		if (bytes.length == 0) {
			stream.writeInt8((byte)128);
			return;
		}
		
		// write bytes regularly
		for (int i = 0; i < bytes.length - 1; i++) {
			byte abyte = bytes[i];
			int intchar = (int)(abyte & 0xff);
			if ((intchar & 0x80) != 0) {
				intchar ^= 0x80;
			}
			stream.writeInt8((byte)intchar);
		}
		// set term flag on last byte
		byte lastbyte = bytes[bytes.length - 1];
		int tchar = (int)(lastbyte & 0xff) | 0x80;
		stream.writeInt8((byte)tchar);
	}

}
