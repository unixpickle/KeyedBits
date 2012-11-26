""" This module provides a means by which to decode KeyedBits objects as Python types. """

import abc
import struct

KB_TYPE_STRING = 1
KB_TYPE_ARRAY = 2
KB_TYPE_DICTIONARY = 3
KB_TYPE_NULL = 4
KB_TYPE_DATA = 5
KB_TYPE_INTEGER = 6
KB_TYPE_FLOAT = 7

class KBTypeError(Exception):
    pass
    
class KBDecodeStream(object):
    """ An abstract base class for KeyedBits decode streams """
    
    __metaclass__ = abc.ABCMeta
    
    @abc.abstractmethod
    def read_byte(self):
        """ Read a byte from the stream or throw an exception """
        pass

    @abc.abstractmethod
    def read_bytearray(self, length):
        """ Read one or more bytes from the stream """
        pass
    
    def read_int(self, byteSize):
        """ Read a signed integer of a given size """
        fmt = {1: 'b', 2: '<h', 4: '<i', 8: '<q'}
        buff = self.read_bytearray(byteSize)
        return struct.unpack(fmt[byteSize], bytes(buff))[0]
        
    def read_uint(self, byteSize):
        """ Read an unsigned integer of a given size """
        fmt = {1: 'B', 2: '<H', 4: '<I', 8: '<Q'}
        buff = self.read_bytearray(byteSize)
        if byteSize == 3:
            newBuff = buff + bytearray([0])
            return struct.unpack('<I', bytes(newBuff))[0]
        else:
            return struct.unpack(fmt[byteSize], bytes(buff))[0]
    
    def read_string(self):
        """ Read a null-terminated string """
        s = ''
        while True:
            c = self.read_byte()
            if c == 0: break
            s += chr(c)
        return s
    
class KBDataDecodeStream(KBDecodeStream):
    """ A KeyedBits decode stream for an in-memory bytearray """
    
    def __init__(self, initial):
        self.data = initial
    
    def read_byte(self):
        if len(self.data) == 0:
            raise Exception('buffer underflow exception')
        else:
            b = self.data[0]
            self.data = self.data[1:]
            return b
    
    def read_bytearray(self, theLen):
        if len(self.data) < theLen:
            raise Exception('buffer underflow exception')
        else:
            b = self.data[:theLen]
            self.data = self.data[theLen:]
            return b

KBDecodeStream.register(KBDataDecodeStream)

class KBDecoder(object):
    """ A class which can decode KeyedBits data into Python types """
    
    def __init__(self, stream):
        self.stream = stream
    
    def read(self):
        """ Read a data value from the encoder's stream in the KeyedBits format.
        
            This may throw a KBTypeError on invalid KeyedBits data
        """
        typeVal = self.stream.read_byte()
        typeObj = KBTypeHeader(typeVal)
        return self.read_with_type(typeObj)
    
    def read_with_type(self, typeObj):
        if typeObj.typeNumber == KB_TYPE_STRING: return self.read_string(typeObj)
        elif typeObj.typeNumber == KB_TYPE_ARRAY: return self.read_array(typeObj)
        elif typeObj.typeNumber == KB_TYPE_DICTIONARY: return self.read_dictionary(typeObj)
        elif typeObj.typeNumber == KB_TYPE_NULL: return None
        elif typeObj.typeNumber == KB_TYPE_DATA: return self.read_data(typeObj)
        elif typeObj.typeNumber == KB_TYPE_INTEGER: return self.read_integer(typeObj)
        elif typeObj.typeNumber == KB_TYPE_FLOAT: return self.read_float(typeObj)
    
    def read_string(self, typeObj):
        if not typeObj.nullTerminated: raise KBTypeError('string must be null terminated')
        return self.stream.read_string()
        
    def read_array(self, typeObj):
        if not typeObj.nullTerminated: raise KBTypeError('array must be null terminated')
        l = list()
        while True:
            typeVal = self.stream.read_byte()
            if typeVal == 0: break
            typeObj = KBTypeHeader(typeVal)
            obj = self.read_with_type(typeObj)
            l = l + [obj]
        return l
    
    def read_dictionary(self, typeObj):
        # oh man i don't wanna do this...
        d = dict()
        while True:
            key = self.read_dictionary_key()
            if key == None: break
            obj = self.read()
            d[key] = obj
        return d
    
    def read_data(self, typeObj):
        dataLength = self.stream.read_uint(typeObj.lenLen)
        return self.stream.read_bytearray(dataLength)
    
    def read_integer(self, typeObj):
        useLen = 1 << typeObj.lenLen # 2^lenLen long
        if useLen == 16: raise KBTypeError('integer must be 32- or 64-bits')
        return self.stream.read_int(useLen)
        
    def read_float(self, typeObj):
        if not typeObj.nullTerminated: raise KBTypeError('float must be null terminated')
        return float(self.stream.read_string())
    
    def read_dictionary_key(self):
        k = ''
        while True:
            c = self.stream.read_byte()
            if c == 0:
                if len(k) == 0: return None
                else: raise KBTypeError('cannot NULL terminate a dictionary key')
            if c & 128:
                k += chr(c ^ 128)
                break
            else: k += chr(c)
        return k
    
class KBTypeHeader(object):
    """ A class used to represent the type of a KeyedBits object as in the binary format. """
    
    def __init__(self, byte):
        self.typeNumber = byte & 7
        self.lenLen = ((byte >> 5) & 3) + 1
        self.nullTerminated = (byte >> 7) & 1 == 1
 