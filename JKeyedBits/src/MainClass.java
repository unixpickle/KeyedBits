import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.aqnichol.keyedbits.decode.ValueDecoder;
import com.aqnichol.keyedbits.encode.ValueEncoder;
import com.aqnichol.keyedbits.value.*;


public class MainClass {
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		testIntegers();
		testDoubles();
		testStrings();
		testData();
		testArrays();
		testDictionary();
		try {
			testFileOutput();
		} catch (IOException e) {
			e.printStackTrace();
		}
		System.out.println("All tests succeeded.");
	}
	
	public static void testIntegers () {
		byte type = Value.createType(Value.KBValueTypeInteger, false, (byte)3);
		byte intStr[] = {type, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)127};
		
		Object numberObj = ValueDecoder.decodeRootObject(intStr);
		if (!(numberObj instanceof Long)) {
			throw new TestFailedException("Invalid decoded integer class:" + numberObj.getClass().getName());
		}
		Long number = (Long)numberObj;

		System.out.println("Decoded number: " + number.longValue());
		if (number.longValue() != Long.MAX_VALUE) {
			throw new TestFailedException("Decoded integer did not match.");
		}
		
		byte[] encoded = ValueEncoder.encodeRootObject(number);
		if (!Arrays.equals(intStr, encoded)) {
			throw new TestFailedException("Integer encoded to un-matching buffer.");
		}
	}
	
	public static void testDoubles () {
		byte type = Value.createType(Value.KBValueTypeFloating, true, (byte)1);
		byte floatStr[] = {type, (byte)'1', (byte)'.', (byte)'5', 0};
		
		Object floatObj = ValueDecoder.decodeRootObject(floatStr);
		if (!(floatObj instanceof Double)) {
			throw new TestFailedException("Invalid decoded integer class:" + floatObj.getClass().getName());
		}
		Double number = (Double)floatObj;
		
		System.out.println("Decoded double: " + number.doubleValue());
		if (number.doubleValue() != 1.5) {
			throw new TestFailedException("Decoded integer did not match.");
		}
		
		byte[] encoded = ValueEncoder.encodeRootObject(number);
		if (!Arrays.equals(floatStr, encoded)) {
			throw new TestFailedException("Double encoded to un-matching buffer.");
		}
	}
	
	public static void testStrings () {
		byte type = Value.createType(Value.KBValueTypeUTF8String, true, (byte)1);
		byte stringStr[] = {type, 't', 'e', 's', 't', 0};
		
		Object stringObj = ValueDecoder.decodeRootObject(stringStr);
		if (!(stringObj instanceof String)) {
			throw new TestFailedException("Invalid decoded string class: " + stringObj.getClass().getName());
		}
		
		String string = (String)stringObj;
		System.out.println("Decoded string: " + string);
		
		byte[] encoded = ValueEncoder.encodeRootObject(string);
		if (!Arrays.equals(encoded, stringStr)) {
			throw new TestFailedException("String encoded to un-matching buffer.");
		}
	}
	
	public static void testData () {
		byte[] small = new byte[255];
		byte[] mediumSmall = new byte[65535];
		byte[] mediumLarge = new byte[0xffffff];
		byte[] large = new byte[0x1000000];
		// populate pieces of data
		for (int i = 0; i < large.length; i++) {
			if (i < small.length) {
				small[i] = (byte)i;
			}
			if (i < mediumSmall.length) {
				mediumSmall[i] = (byte)i;
			}
			if (i < mediumLarge.length) {
				mediumLarge[i] = (byte)i;
			}
			large[i] = (byte)i;
		}
		
		// encode & decode
		byte[] encoded = ValueEncoder.encodeRootObject(ByteBuffer.wrap(small));
		ByteBuffer decoded = (ByteBuffer)ValueDecoder.decodeRootObject(encoded);
		if (!Arrays.equals(decoded.array(), small)) {
			throw new TestFailedException("encode/decode of small buffer failed");
		}
		
		System.out.println("Decoded data: small data");
		
		encoded = ValueEncoder.encodeRootObject(ByteBuffer.wrap(mediumSmall));
		decoded = (ByteBuffer)ValueDecoder.decodeRootObject(encoded);
		if (!Arrays.equals(decoded.array(), mediumSmall)) {
			throw new TestFailedException("encode/decode of medium small buffer failed");
		}
		
		System.out.println("Decoded data: medium-small data");
		
		encoded = ValueEncoder.encodeRootObject(ByteBuffer.wrap(mediumLarge));
		decoded = (ByteBuffer)ValueDecoder.decodeRootObject(encoded);
		if (!Arrays.equals(decoded.array(), mediumLarge)) {
			throw new TestFailedException("encode/decode of medium large buffer failed");
		}

		System.out.println("Decoded data: medium-large data");
		
		encoded = ValueEncoder.encodeRootObject(ByteBuffer.wrap(large));
		decoded = (ByteBuffer)ValueDecoder.decodeRootObject(encoded);
		if (!Arrays.equals(decoded.array(), large)) {
			throw new TestFailedException("encode/decode of large buffer failed");
		}
		
		System.out.println("Decoded data: large data");
	}
	
	public static void testArrays () {
		ArrayList<Object> objects = new ArrayList<Object>();
		objects.add(new Integer(1337));
		objects.add(new Integer(-1337));
		objects.add(new Double(3.14));
		objects.add(null);
		objects.add("My name is Jimmy");
		
		byte[] encodedBytes = ValueEncoder.encodeRootObject(objects);
		Object decoded = ValueDecoder.decodeRootObject(encodedBytes);
		
		if (!(decoded instanceof ArrayList)) {
			throw new TestFailedException("Decoded array is of non-list class: " + decoded.getClass().getName());
		}
		
		@SuppressWarnings("unchecked")
		ArrayList<Object> list = (ArrayList<Object>)decoded;
		
		System.out.println("Decoded array: " + list);
		
		for (int i = 0; i < list.size(); i++) {
			Object object = list.get(i);
			Object oldObject = objects.get(i);
			if (object == null && oldObject == null) {
				
			} else {
				if (!list.get(i).equals(objects.get(i))) {
					throw new TestFailedException("Object at array index " + i + " is different.");
				}
			}
		}
	}
	
	public static void testDictionary () {
		ArrayList<Object> colorArray = new ArrayList<Object>();
		ArrayList<Object> petsArray = new ArrayList<Object>();
		HashMap<String, Object> petDict = new HashMap<String, Object>();
		colorArray.add("blue");
		colorArray.add("green");
		colorArray.add("purple");
		petDict.put("name", "Binary");
		petDict.put("species", "Dog");
		petsArray.add(petDict);
		
		HashMap<String, Object> map = new HashMap<String, Object>();
		map.put("colors", colorArray);
		map.put("name", "Alex");
		map.put("pets", petsArray);
		map.put("age", new Integer(14));
		
		byte[] encoded = ValueEncoder.encodeRootObject(map);
		Object decodedObj = ValueDecoder.decodeRootObject(encoded);
		if (!(decodedObj instanceof Map)) {
			throw new TestFailedException("Invalid decoded map class:" + decodedObj.getClass().getName());
		}
		
		@SuppressWarnings("unchecked")
		Map<String, Object> decoded = (Map<String, Object>)decodedObj;
		System.out.println("Decoded map: " + decoded);
		if (!deepCompareDict(decoded, map)) {
			throw new TestFailedException("Decoded map is un-equal to original");
		}
	}
	
	public static void testFileOutput () throws IOException {
		ArrayList<Object> testArray = new ArrayList<Object>();
		testArray.add("Test string");
		testArray.add(new Integer(1337));
		testArray.add("Another string");
		
		File temp = File.createTempFile("outputfile", ".kb");
	    temp.deleteOnExit();
	    
	    FileOutputStream stream = new FileOutputStream(temp);
	    ValueEncoder.encodeRootObjectToStream(testArray, stream);
	    stream.close();
	    
	    FileInputStream input = new FileInputStream(temp);
	    Object dec = ValueDecoder.decodeRootObjectFromStream(input);
	    input.close();
	    
	    System.out.println("Decoded array from file: " + testArray);
	    if (!dec.equals(testArray)) {
	    	throw new TestFailedException("Decoded array from file differs from original.");
	    }
	}
	
	public static boolean deepCompareDict (Map<String, Object> map1, Map<String, Object> map2) {
		Set<String> keys = map1.keySet();
		Set<String> keys1 = map2.keySet();
		if (keys.size() != keys1.size()) {
			System.out.println("Dickhead101");
			return false;
		}
		for (String key : keys) {
			if (!keys1.contains(key)) {
				System.out.println("No can haz key: " + key);
				return false;
			}
		}
		for (String key : keys) {
			Object o1 = map1.get(key);
			Object o2 = map2.get(key);
			if (!deepCompareObjects(o1, o2)) {
				return false;
			}
		}
		return true;
	}
	
	public static boolean deepCompareArray (ArrayList<Object> list1, ArrayList<Object> list2) {
		if (list1.size() != list2.size()) {
			return false;
		}
		for (int i = 0; i < list1.size(); i++) {
			Object o1 = list1.get(i);
			Object o2 = list2.get(i);
			if (!deepCompareObjects(o1, o2)) {
				return false;
			}
		}
		
		return true;
	}
	
	@SuppressWarnings("unchecked")
	public static boolean deepCompareObjects (Object o1, Object o2) {
		if (!o1.getClass().isInstance(o2)) {
			System.out.println("NO can haz equals class");
			return false;
		}
		if (o1 instanceof Map && o2 instanceof Map) {
			if (!deepCompareDict((Map<String, Object>)o1, (Map<String, Object>)o2)) {
				System.out.println("NO can haz equals");
				return false;
			}
		} else if (o1 instanceof ArrayList && o2 instanceof ArrayList) {
			if (!deepCompareArray((ArrayList<Object>)o1, (ArrayList<Object>)o2)) {
				System.out.println("NO can has equals");
				return false;
			}
		} else {
			if (!o1.equals(o2)) {
				System.out.println("NO can haz equals()");
				return false;
			}
		}
		return true;
	}

}
