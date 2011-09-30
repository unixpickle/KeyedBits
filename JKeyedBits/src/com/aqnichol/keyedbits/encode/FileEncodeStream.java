package com.aqnichol.keyedbits.encode;

import java.io.IOException;
import java.io.OutputStream;

public class FileEncodeStream extends EncodeStream {

	private OutputStream stream;
	
	public FileEncodeStream (OutputStream stream) {
		this.stream = stream;
	}
	
	public OutputStream getOutputStream () {
		return stream;
	}
	
	public void writeBytes(byte[] theBytes, int offset, int length) {
		try {
			stream.write(theBytes, offset, length);
		} catch (IOException e) {
			throw new EncodeStreamWriteError("Failed to write to the output stream.", e);
		}
	}

	public void closeStream() {
		try {
			stream.close();
		} catch (IOException e) {
			throw new EncodeStreamWriteError("Failed to close the output stream.", e);
		}
	}

}
