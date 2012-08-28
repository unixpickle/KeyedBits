package com.aqnichol.keyedbits.decode;

import java.io.IOException;
import java.io.InputStream;

public class FileDecodeStream extends DecodeStream {

	private InputStream stream;
	
	public FileDecodeStream (InputStream stream) {
		this.stream = stream;
	}
	
	public InputStream getInputStream () {
		return stream;
	}
	
	public byte[] readBytes (int length) {
		byte[] bytes = new byte[length];
		int hasBytes = 0;
		while (hasBytes < length) {
			int read = 0;
			try {
				read = stream.read(bytes, hasBytes, length - hasBytes);
			} catch (IOException e) {
				throw new DecodeStreamReadError("Failed to read from input stream.", e);
			}
			if (read < 0) {
				throw new DecodeStreamReadError("Failed to read from input stream.", null);
			}
			hasBytes += read;
		}
		return bytes;
	}

	public void closeStream() {
		try {
			stream.close();
		} catch (IOException e) {
			throw new DecodeStreamReadError("Failed to close input stream.", e);
		}
	}

}
