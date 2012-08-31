module KeyedBits.Integer (
    encodeWord,
    encodeInt,
    decodeWord,
    decodeInt,
    minLenInt,
    minLenWord
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Binary as Binary
import Data.Word
import Data.Int

flipLazyToStrict :: Lazy.ByteString -> BS.ByteString
flipLazyToStrict x = BS.pack $ reverse $ Lazy.unpack x

flipStrictToLazy :: BS.ByteString -> Lazy.ByteString
flipStrictToLazy x = Lazy.pack $ reverse $ BS.unpack x

encodeWord24 :: (Integral a) => a -> BS.ByteString
encodeWord24 x = BS.take 3 $ flipLazyToStrict $ Binary.encode (fromIntegral x :: Word32)

decodeWord24 :: BS.ByteString -> Int
decodeWord24 x = fromIntegral word :: Int
    where word = (Binary.decode $ Lazy.cons 0 $ flipStrictToLazy x) :: Word32

encode :: (Binary.Binary a) => a -> BS.ByteString
encode = flipLazyToStrict . Binary.encode

decode :: (Binary.Binary a) => BS.ByteString -> a
decode = Binary.decode . flipStrictToLazy

encodeWord :: (Integral a, Integral b) => a -> b -> BS.ByteString
encodeWord l n
    | l == 1    = encode ((fromIntegral n) :: Word8)
    | l == 2    = encode ((fromIntegral n) :: Word16)
    | l == 3    = encodeWord24 n
    | l == 4    = encode ((fromIntegral n) :: Word32)
    | otherwise = encode ((fromIntegral n) :: Word64)

encodeInt :: (Integral a, Integral b) => a -> b -> BS.ByteString
encodeInt l n
    | l == 1    = encode ((fromIntegral n) :: Int8)
    | l == 2    = encode ((fromIntegral n) :: Int16)
    | l == 4    = encode ((fromIntegral n) :: Int32)
    | otherwise = encode ((fromIntegral n) :: Int64)

decodeWord :: BS.ByteString -> Int
decodeWord b
    | l == 1    = fromIntegral (decode b :: Word8)
    | l == 2    = fromIntegral (decode b :: Word16)
    | l == 3    = decodeWord24 b
    | l == 4    = fromIntegral (decode b :: Word32)
    | otherwise = fromIntegral (decode b :: Word64)
    where l = BS.length b

decodeInt :: BS.ByteString -> Int
decodeInt b
    | l == 1    = fromIntegral (decode b :: Int8)
    | l == 2    = fromIntegral (decode b :: Int16)
    | l == 4    = fromIntegral (decode b :: Int32)
    | otherwise = fromIntegral (decode b :: Int64)
    where l = BS.length b

minLenInt :: (Integral a) => a -> Int
minLenInt n
    | n < 2^7 && n >= -(2^7)   = 1
    | n < 2^15 && n >= -(2^15) = 2
    | n < 2^31 && n >= -(2^31) = 4
    | otherwise = 8

minLenWord :: (Integral a) => a -> Int
minLenWord n
    | n < 2^8   = 1
    | n < 2^16  = 2
    | n < 2^32  = 3
    | otherwise = 4
