package com.aqnichol.keyedbits.decode;

import java.nio.BufferUnderflowException;

public class ByteDecodeStream extends DecodeStream {

	private byte[] bytes;
	private int offset;
	
	public ByteDecodeStream (byte[] theBytes) {
		bytes = theBytes.clone();
		offset = 0;
	}
	
	public void closeStream () {
		// no work needs to be done
	}

	public byte[] readBytes (int length) throws BufferUnderflowException {
		if (offset + length > bytes.length) {
			throw new BufferUnderflowException();
		}
		byte[] retBuf = new byte[length];
		for (int i = 0; i < length; i++) {
			retBuf[i] = bytes[i + offset];
		}
		offset += length;
		return retBuf;
	}

}
