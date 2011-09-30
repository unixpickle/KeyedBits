package com.aqnichol.keyedbits.encode;

import java.io.IOException;

public class EncodeStreamWriteError extends RuntimeException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 188374338061248463L;
	private IOException originalException;
	
	public EncodeStreamWriteError (String message, IOException original) {
		super(message);
		originalException = original;
	}
	
	public IOException getOriginalException () {
		return originalException;
	}
	
}
