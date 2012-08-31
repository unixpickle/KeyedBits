module KeyedBits.Decode (
    decode
) where

import qualified KeyedBits.Integer as KBI
import qualified KeyedBits.Object as KBO
import qualified KeyedBits.Header as KBH
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import Data.Char
import Data.Bits
import Data.Int
import Control.Exception
import Data.Typeable

data DecodeException = BufferUnderflow deriving (Show)

instance Typeable DecodeException where
    typeOf _ = typeOf (undefined :: DecodeException)

instance Exception DecodeException

lazyToStrict :: BS.ByteString -> BSS.ByteString
lazyToStrict x = BSS.pack $ BS.unpack x

decode :: BS.ByteString -> (KBO.KBObject, BS.ByteString)
decode b
    | BS.length b == 0 = throw BufferUnderflow
    | t == KBH.HeaderString = readString head body
    | t == KBH.HeaderInteger = readInt head body
    | t == KBH.HeaderNULL = readNull head body
    | t == KBH.HeaderArray = readArray head body
    | t == KBH.HeaderDictionary = readDictionary head body
    | t == KBH.HeaderData = readData head body
    | t == KBH.HeaderFloat = KeyedBits.Decode.readFloat head body
    | otherwise            = (KBO.KBEmpty, body)
    where head = KBH.fromByte $ BS.index b 0
          body = BS.drop 1 b
          t    = KBH.headerType head

readString :: KBH.Header -> BS.ByteString -> (KBO.KBObject, BS.ByteString)
readString _ b = (KBO.KBString str, remaining)
    where bytes = BS.takeWhile (/= 0) b
          remaining = BS.drop ((BS.length bytes) + 1) b
          str = map (chr . fromIntegral) $ BS.unpack bytes

readInt :: KBH.Header -> BS.ByteString -> (KBO.KBObject, BS.ByteString)
readInt (KBH.Header _ l _) b = (KBO.KBInt number, BS.drop length b)
    where length = if l > 1 then 8 else 4
          bytes = BS.take length b
          number = KBI.decodeInt $ lazyToStrict bytes

readNull :: KBH.Header -> BS.ByteString -> (KBO.KBObject, BS.ByteString)
readNull _ b = (KBO.KBNull, b)

readData :: KBH.Header -> BS.ByteString -> (KBO.KBObject, BS.ByteString)
readData (KBH.Header _ l _) b = (KBO.KBData $ BS.unpack contents, left)
    where lenLen = (fromIntegral l) :: Int64
          bytes = BS.take (lenLen + 1) b
          length = KBI.decodeWord $ lazyToStrict bytes
          remaining = BS.drop (lenLen + 1) b
          (contents, left) = BS.splitAt (fromIntegral length) remaining
          

readFloat :: KBH.Header -> BS.ByteString -> (KBO.KBObject, BS.ByteString)
readFloat _ b = (KBO.KBFloat num, remaining)
    where num = read str :: Float
          bytes = BS.takeWhile (/= 0) b
          remaining = BS.drop ((BS.length bytes) + 1) b
          str = map (chr . fromIntegral) $ BS.unpack bytes

readArray :: KBH.Header -> BS.ByteString -> (KBO.KBObject, BS.ByteString)
readArray t b
    | BS.length b == 0 = throw BufferUnderflow
    | isTermed         = (KBO.KBEmpty, remaining)
    | otherwise        = let (nextObj, left) = readArray t remaining
                         in (KBO.KBArray obj nextObj, left)
    where (obj, remaining) = decode b
          isTermed = obj == KBO.KBEmpty

readDictionary :: KBH.Header -> BS.ByteString -> (KBO.KBObject, BS.ByteString)
readDictionary t x
    | BS.length x == 0 = throw BufferUnderflow
    | isTermed         = (KBO.KBEmpty, remaining)
    | otherwise        = let (obj, trimmed) = decode remaining
                             (nextDict, buf) = readDictionary t trimmed
                         in (KBO.KBHash key obj nextDict, buf)
    where (key, remaining) = dictKey x
          isTermed = (length key == 0)

dictKey :: BS.ByteString -> (String, BS.ByteString)
dictKey b
    | BS.length b == 0 = ([], b)
    | clean == 0       = ([], BS.drop 1 b)
    | clean /= byte    = ([letter], BS.drop 1 b)
    | otherwise        = let (next, buff) = dictKey $ BS.drop 1 b
                         in ((letter:next), buff)
    where byte = (fromIntegral $ BS.index b 0) :: Int
          clean = if byte .&. 0x80 == 0 then byte else byte `xor` 0x80
          letter = chr clean
