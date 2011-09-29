package com.aqnichol.keyedbits;

import java.nio.charset.Charset;
import java.nio.charset.UnsupportedCharsetException;

public class ByteBuilder {

	private int bufferSize;
	private int offset;
	private byte[] bytes;
	
	public ByteBuilder (int bufferSize) {
		this.bufferSize = bufferSize;
		offset = 0;
		bytes = new byte[bufferSize];
	}
	
	public void appendBytes (byte[] someBytes) {
		appendBytes(someBytes, 0, someBytes.length);
	}
	
	public void appendBytes (byte[] someBytes, int offset, int length) {
		int newLength = this.offset + length;
		if (newLength > bytes.length) {
			// we need to do a little reallocating.
			int addOn = newLength % bufferSize;
			if (addOn != 0) {
				newLength += (bufferSize - addOn);
			}
			byte[] newBytes = new byte[newLength];
			for (int i = 0; i < this.offset; i++) {
				newBytes[i] = bytes[i];
			}
			bytes = newBytes;
		}
		for (int i = 0; i < length; i++) {
			byte srcByte = someBytes[offset + i];
			bytes[i + this.offset] = srcByte;
		}
		this.offset += length;
	}
	
	public byte[] getBytes () {
		return bytes;
	}
	
	public byte[] getUsedBytes () {
		byte[] newBytes = new byte[offset];
		for (int i = 0; i < offset; i++) {
			newBytes[i] = bytes[i];
		}
		return newBytes;
	}
	
	public String decodeUTF8String () {
		try {
			return new String (bytes, 0, offset, Charset.forName("UTF-8"));
		} catch (UnsupportedCharsetException e) {
			return null;
		}
	}
	
}
