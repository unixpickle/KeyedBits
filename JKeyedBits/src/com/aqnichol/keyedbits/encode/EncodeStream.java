package com.aqnichol.keyedbits.encode;

public abstract class EncodeStream {

	public abstract void writeBytes (byte[] theBytes, int offset, int length);
	public abstract void closeStream ();
	
}
