module KeyedBits.Encode (
    encode
) where

import qualified KeyedBits.Object as KBO
import qualified KeyedBits.Header as KBH
import qualified KeyedBits.Integer as KBI

import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Bits
import Data.Word

header :: KBO.KBObject -> KBH.Header
header (KBO.KBString _) = KBH.Header KBH.HeaderString 0 True
header (KBO.KBInt i)
    | KBI.minLenInt i > 4 = KBH.Header KBH.HeaderInteger 2 True
    | otherwise           = KBH.Header KBH.HeaderInteger 1 True
header (KBO.KBData buffer) = KBH.Header KBH.HeaderData (bytes - 1) False
    where bytes = KBI.minLenWord $ length buffer
header (KBO.KBFloat _) = KBH.Header KBH.HeaderFloat 0 True
header (KBO.KBNull) = KBH.Header KBH.HeaderNULL 0 True
header (KBO.KBArray _ _) = KBH.Header KBH.HeaderArray 0 True
header (KBO.KBHash _ _ _) = KBH.Header KBH.HeaderDictionary 0 True
header _ = KBH.Header KBH.HeaderTerminator 0 False

headByte :: KBO.KBObject -> Word8
headByte = KBH.toByte . header

encode :: KBO.KBObject -> BS.ByteString
encode obj@(KBO.KBEmpty) = BS.singleton $ headByte obj
encode obj@(KBO.KBNull) = BS.singleton $ headByte obj
encode obj@(KBO.KBString s) = buffer
    where str = map (fromIntegral . ord) s
          buffer = BS.cons (headByte obj) $ BS.pack $ str ++ [0]
encode obj@(KBO.KBInt i) = buffer
    where length = max 4 $ KBI.minLenInt i
          buffer = BS.cons (headByte obj) $ KBI.encodeInt length i
encode obj@(KBO.KBFloat f) = buffer
    where str = map (fromIntegral . ord) $ show f
          buffer = BS.cons (headByte obj) (BS.pack $ str ++ [0])
encode obj@(KBO.KBArray x xs) = BS.cons (headByte obj) $ encodeArr obj
encode obj@(KBO.KBHash k v n) = BS.cons (headByte obj) $ encodeDict obj
encode obj@(KBO.KBData b) = BS.cons (headByte obj) $ body
    where lenbytes = KBI.encodeWord (KBI.minLenWord $ length b) $ length b
          body = BS.append lenbytes $ BS.pack b

encodeArr :: KBO.KBObject -> BS.ByteString
encodeArr (KBO.KBArray x xs) = BS.append (encode x) $ encodeArr xs
encodeArr _ = BS.singleton 0

encodeDict :: KBO.KBObject -> BS.ByteString
encodeDict obj@(KBO.KBHash k v n) = BS.append body $ encodeDict n
    where key = BS.pack $ encodeKey k
          body = BS.append key $ encode v
encodeDict _ = BS.singleton 0

encodeKey :: String -> [Word8]
encodeKey (x:[]) = let ch = (fromIntegral $ ord x) :: Word8
                   in [ch .|. (0x80 :: Word8)]
encodeKey (x:xs) = (fromIntegral $ ord x):(encodeKey xs)
