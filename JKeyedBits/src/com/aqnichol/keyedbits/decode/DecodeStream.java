package com.aqnichol.keyedbits.decode;

public abstract class DecodeStream {

	public abstract byte[] readBytes (int length);
	public abstract void closeStream ();
	
}
