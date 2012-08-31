module KeyedBits.Header (
    HeaderType(..),
    Header(..),
    fromByte,
    toByte,
    headerType,
    lengthLength,
    isNullTerminated
) where

import Data.Bits
import Data.Word

data HeaderType = HeaderTerminator | HeaderString | HeaderInteger | HeaderFloat | HeaderData | HeaderNULL | HeaderArray | HeaderDictionary deriving (Show, Eq)
data Header = Header HeaderType Int Bool deriving (Show)

fromByte :: Word8 -> Header
fromByte byte = Header t l n
    where l = fromIntegral $ (byte `shift` (-5)) .&. 3
          n = if (byte `shift` (-7)) .&. 1 == 1 then True else False
          t = case (byte .&. 7) of
                   0 -> HeaderTerminator
                   1 -> HeaderString
                   2 -> HeaderArray
                   3 -> HeaderDictionary
                   4 -> HeaderNULL
                   5 -> HeaderData
                   6 -> HeaderInteger
                   7 -> HeaderFloat

toByte :: Header -> Word8
toByte (Header tval lval nval) = fromIntegral $ l .|. n .|. t
    where l = (lval .&. 3) `shift` 5
          n = if nval then 128 else 0
          t = case tval of
                   HeaderTerminator -> 0
                   HeaderString -> 1
                   HeaderArray -> 2
                   HeaderDictionary -> 3
                   HeaderNULL -> 4
                   HeaderData -> 5
                   HeaderInteger -> 6
                   HeaderFloat -> 7

headerType :: Header -> HeaderType
headerType (Header t _ _) = t

lengthLength :: Header -> Int
lengthLength (Header _ l _) = l

isNullTerminated :: Header -> Bool
isNullTerminated (Header _ _ b) = b
