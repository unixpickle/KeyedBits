package com.aqnichol.keyedbits.decode;

import com.aqnichol.keyedbits.ByteBuilder;

public class DecodeStreamReader {

	private DecodeStream stream;
	
	public DecodeStreamReader (DecodeStream aStream) {
		stream = aStream;
	}
	
	public DecodeStream getStream () {
		return stream;
	}
	
	public void closeStream () {
		stream.closeStream();
	}
	
	public byte readUInt8 () {
		byte[] aByte = stream.readBytes(1);
		return aByte[0];
	}
	
	public short readInt8 () {
		return (short)readUInt8();
	}
	
	public int readUInt16 () {
		// assuming little endian input
		byte[] bytes = stream.readBytes(2);
		int anInt = ((bytes[1] & 0xff) << 8) | (bytes[0] & 0xff);
		return anInt;
	}
	
	public short readInt16 () {
		return (short)readUInt16();
	}
	
	public int readUInt24 () {
		byte[] bytes = stream.readBytes(3);
		int anInt = ((bytes[2] & 0xff) << 16) | ((bytes[1] & 0xff) << 8) | (bytes[0] & 0xff);
		return anInt;
	}
	
	public long readUInt32 () {
		byte[] bytes = stream.readBytes(4);
		long anInt = ((bytes[3] & 0xff) << 24) | ((bytes[2] & 0xff) << 16) | ((bytes[1] & 0xff) << 8) | (bytes[0] & 0xff);
		return anInt;
	}
	
	public int readInt32 () {
		return (int)readUInt32();
	}
	
	public long readInt64 () {
		int i1 = readInt32();
		int i2 = readInt32();
		long num = i2 & 0xffffffffL;
		num <<= 32L;
		num |= i1 & 0xffffffffL;
		return num;
	}
	
	public String readNullTerminatedString () {
		ByteBuilder builder = new ByteBuilder(512);
		while (true) {
			byte buff[] = stream.readBytes(1);
			if (buff[0] == 0) {
				break;
			} else {
				builder.appendBytes(buff);
			}
		}
		return builder.decodeUTF8String();
	}
	
}
