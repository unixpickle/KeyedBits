package com.aqnichol.keyedbits.value;

import java.nio.ByteBuffer;

import com.aqnichol.keyedbits.decode.DecodeStreamReader;
import com.aqnichol.keyedbits.encode.EncodeStreamWriter;

public class DataValue extends Value {
	
	public DataValue (DecodeStreamReader stream, byte type) {
		super(stream, type);
		decodeFromStream(stream);
	}
	
	public DataValue (Object object) {
		super(object);
		if (!(object instanceof ByteBuffer)) {
			throw new UnmatchedTypeException("Invalid bytes class: " + object.getClass().getName());
		}
		ByteBuffer bb = (ByteBuffer)object;
		byte[] bytes = bb.array();
		int lenLength = 1;
		if (bytes.length > 0xffffff) {
			lenLength = 4;
		} else if (bytes.length > 0xffff) {
			lenLength = 3;
		} else if (bytes.length > 0xff) {
			lenLength = 2;
		}
		type = Value.createType(KBValueTypeData, false, (byte)lenLength);
	}
	
	public ByteBuffer getByteBuffer () {
		return (ByteBuffer)object;
	}
	
	protected void decodeFromStream (DecodeStreamReader stream) {
		long dataLength = 0;
		int lenLen = this.getTypeLengthFieldLength();
		switch (lenLen) {
			case 1:
				dataLength = (stream.readUInt8() & 0xff);
				break;
			case 2:
				dataLength = stream.readUInt16();
				break;
			case 3:
				dataLength = stream.readUInt24();
				break;
			case 4:
				dataLength = stream.readInt32();
				break;
			default:
				throw new UnmatchedTypeException("Type has invalid length field: " + type);
		}
		byte[] bytes = stream.getStream().readBytes((int)dataLength);
		object = ByteBuffer.wrap(bytes);
	}

	public void encodeToStream (EncodeStreamWriter stream) {
		stream.writeInt8(type);
		ByteBuffer bb = getByteBuffer();
		byte[] bytes = bb.array();
		int lenLen = this.getTypeLengthFieldLength();
		switch (lenLen) {
			case 1:
				stream.writeUInt8((short)bytes.length);
				break;
			case 2:
				stream.writeUInt16((int)bytes.length);
				break;
			case 3:
				stream.writeUInt24((int)bytes.length);
				break;
			case 4:
				stream.writeUInt32((long)bytes.length);
				break;
			default:
				throw new UnmatchedTypeException("Type has invalid length field: " + (type & 0xff));
		}
		stream.getStream().writeBytes(bytes, 0, bytes.length);
	}
}
