package com.aqnichol.keyedbits.encode;

import java.nio.charset.Charset;

public class EncodeStreamWriter {

	private EncodeStream stream;
	
	public EncodeStreamWriter (EncodeStream stream) {
		this.stream = stream;
	}
	
	public EncodeStream getStream () {
		return stream;
	}
	
	public void closeStream () {
		stream.closeStream();
	}
	
	public void writeUInt8 (short number) {
		writeInt8((byte)number);
	}
	
	public void writeInt8 (byte number) {
		byte[] b = {number};
		stream.writeBytes(b, 0, 1);
	}
	
	public void writeUInt16 (int number) {
		writeInt16((short)(number & 0xffff));
	}
	
	public void writeInt16 (short number) {
		byte b1 = (byte)(number & 0xff);
		byte b2 = (byte)((number >> 8) & 0xff);
		byte[] b = {b1, b2};
		stream.writeBytes(b, 0, 2);
	}
	
	public void writeUInt24 (int number) {
		byte b1 = (byte)(number & 0xff);
		byte b2 = (byte)((number >> 8) & 0xff);
		byte b3 = (byte)((number >> 16) & 0xff);
		byte[] b = {b1, b2, b3};
		stream.writeBytes(b, 0, 3);
	}
	
	public void writeUInt32 (long number) {
		writeInt32((int)number);
	}
	
	public void writeInt32 (int number) {
		byte b1 = (byte)(number & 0xff);
		byte b2 = (byte)((number >> 8) & 0xff);
		byte b3 = (byte)((number >> 16) & 0xff);
		byte b4 = (byte)((number >> 24) & 0xff);
		byte[] b = {b1, b2, b3, b4};
		stream.writeBytes(b, 0, 4);
	}
	
	public void writeInt64 (long number) {
		byte b1 = (byte)(number & 0xff);
		byte b2 = (byte)((number >> 8) & 0xff);
		byte b3 = (byte)((number >> 16) & 0xff);
		byte b4 = (byte)((number >> 24) & 0xff);
		byte b5 = (byte)((number >> 32) & 0xff);
		byte b6 = (byte)((number >> 40) & 0xff);
		byte b7 = (byte)((number >> 48) & 0xff);
		byte b8 = (byte)((number >> 56) & 0xff);
		byte[] b = {b1, b2, b3, b4, b5, b6, b7, b8};
		stream.writeBytes(b, 0, 8);
	}
	
	public void writeNullTerminated (String string) {
		byte[] data = string.getBytes(Charset.forName("UTF-8"));
		byte[] term = {0};
		stream.writeBytes(data, 0, data.length);
		stream.writeBytes(term, 0, 1);
	}
	
}
