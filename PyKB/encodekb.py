""" This module provides a simple way to encode Python types as KeyedBits """

import abc
import struct
from types import NoneType

KB_TYPE_STRING = 1
KB_TYPE_ARRAY = 2
KB_TYPE_DICTIONARY = 3
KB_TYPE_NULL = 4
KB_TYPE_DATA = 5
KB_TYPE_INTEGER = 6
KB_TYPE_FLOAT = 7

class KBEncodeStream(object):
    """ An abstract base class for KeyedBits encode streams """

    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def write_byte(self, byte):
        """ Write a byte of data to the stream """
        pass

    @abc.abstractmethod
    def write_bytearray(self, data):
        """ Write a bytearray to the stream """
        pass

    def write_int(self, number, byteSize):
        fmts = {1: 'b', 2: '<h', 4: '<i', 8: '<q'}
        buff = struct.pack(fmts[byteSize], number)
        self.write_bytearray(bytearray(buff))

    def write_uint(self, number, byteSize):
        fmts = {1: 'B', 2: '<H', 4: '<I', 8: '<Q'}
        if byteSize == 3:
            data = struct.pack('<I', number)
            self.write_bytearray(bytearray(data[:-1]))
        else:
            buff = struct.pack(fmts[byteSize], number)
            self.write_bytearray(bytearray(buff))

class KBDataEncodeStream(KBEncodeStream):
    """ A KeyedBits encode stream for an in-memory bytearray """

    def __init__(self, initial=bytearray()):
        self.data = initial

    def write_byte(self, byte):
        self.data.append(byte)

    def write_bytearray(self, data):
        self.data += data

KBEncodeStream.register(KBDataEncodeStream)

class KBEncoder(object):
    """ A class which can encode the various KeyedBits types """

    def __init__(self, stream):
        self.stream = stream

    def write(self, value):
        """ Write a data value to the encoder's stream in the KeyedBits format 
        
            This is the only encode method that should be called externally.
        """
        if type(value) is int: self.write_int(value)
        elif type(value) is str: self.write_str(value)
        elif type(value) is float: self.write_float(value)
        elif type(value) is bool: self.write_int(1 if value else 0)
        elif type(value) is list: self.write_list(value)
        elif type(value) is dict: self.write_dictionary(value)
        elif type(value) is bytearray: self.write_bytearray(value)
        elif type(value) is NoneType: self.write_type(1, 1, KB_TYPE_NULL)
        else: raise TypeError, 'the provided type is not KeyedBits compliant'

    def write_type(self, lenLen, nullTerm, typeNum):
        lenLenLarge = (lenLen - 1) & 3
        typeVal = 1 << 7 if nullTerm else 0
        typeVal |= (lenLenLarge << 5)
        typeVal |= typeNum & 7
        self.stream.write_byte(typeVal)

    def write_int(self, integer):
        if integer < 2**31 and integer >= -2**31:
            # write it as a 32-bit value
            self.write_type(2, 1, KB_TYPE_INTEGER)
            self.stream.write_int(integer, 4)
        else:
            # write it as a 64-bit value
            self.write_type(3, 1, KB_TYPE_INTEGER)
            self.stream.write_int(integer, 8)

    def write_str(self, buff):
        self.write_type(1, 1, KB_TYPE_STRING)
        self.stream.write_bytearray(bytearray(buff.encode('utf-8')))
        self.stream.write_byte(0)

    def write_float(self, value):
        self.write_type(1, 1, KB_TYPE_FLOAT)
        self.stream.write_bytearray(bytearray(str(value)))
        self.stream.write_byte(0)

    def write_list(self, value):
        self.write_type(1, 1, KB_TYPE_ARRAY)
        for obj in value:
            self.write(obj)
        self.stream.write_byte(0)

    def write_dictionary(self, dict):
        self.write_type(1, 1, KB_TYPE_DICTIONARY)
        for key in dict:
            self.write_dictionary_key(key)
            self.write(dict[key])
        self.stream.write_byte(0)

    def write_bytearray(self, buf):
        lenLen = 0
        if len(buf) < 0xff: lenLen = 1
        elif len(buf) < 0xffff: lenLen = 2
        elif len(buf) < 0xffffffff: lenLen = 3
        else: lenLen = 4
        self.write_type(lenLen, 0, KB_TYPE_DATA)
        self.stream.write_uint(len(buf), lenLen)
        self.stream.write_bytearray(buf)

    def write_dictionary_key(self, key):
        if not type(key) is str: raise TypeError, 'dictionary key must be a string'
        if len(key) == 0: raise ValueError, 'dictionary key cannot be empty'
        for i in range(0, len(key)):
            val = ord(key[i])
            val &= 127
            if i + 1 == len(key): val |= 128
            self.stream.write_byte(val)
