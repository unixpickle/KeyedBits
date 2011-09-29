package com.aqnichol.keyedbits.encode;

import com.aqnichol.keyedbits.ByteBuilder;

public class ByteEncodeStream extends EncodeStream {

	private ByteBuilder builder;
	
	public ByteEncodeStream () {
		builder = new ByteBuilder(512);
	}

	public void writeBytes(byte[] theBytes, int offset, int length) {
		builder.appendBytes(theBytes, offset, length);
	}

	public void closeStream() {
		// nothing to do
	}
	
	public byte[] getBytes () {
		return builder.getUsedBytes();
	}
	
}
