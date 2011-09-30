package com.aqnichol.keyedbits.decode;

import java.io.IOException;

public class DecodeStreamReadError extends RuntimeException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 3402587897358097460L;
	private IOException originalException;
	
	public DecodeStreamReadError (String message, IOException original) {
		super(message);
		originalException = original;
	}
	
	public IOException getOriginalException () {
		return originalException;
	}
	
}
