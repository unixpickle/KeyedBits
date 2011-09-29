package com.aqnichol.keyedbits.decode;

import java.nio.BufferUnderflowException;

public abstract class DecodeStream {

	public abstract byte[] readBytes (int length) throws BufferUnderflowException;
	public abstract void closeStream ();
	
}
