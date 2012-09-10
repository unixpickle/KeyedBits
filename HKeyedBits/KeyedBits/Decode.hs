module KeyedBits.Decode (
    runDecodeState,
    readObject,
    DecodeException(..)
) where

import Control.Exception
import Data.Typeable

import qualified KeyedBits.Integer as KBI
import qualified KeyedBits.Object as KBO
import qualified KeyedBits.Header as KBH

import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Bits
import Data.Int
import Control.Monad
import Control.Applicative

data DecodeException = BufferUnderflowException deriving (Show, Typeable)
instance Exception DecodeException

newtype DecodeState s a = DecodeState { runDecodeState :: s -> (a, s) }

instance Monad (DecodeState s) where
    return o = DecodeState $ (\x -> (o, x))
    a >>= b = DecodeState $ \x ->
        let all@(v, s) = runDecodeState a x
        in runDecodeState (b v) s

instance Functor (DecodeState s) where
    fmap f s = DecodeState $ \x ->
        let (v, n) = runDecodeState s x
        in (f v, n)

instance Applicative (DecodeState s) where
    pure = return
    (<*>) = ap

readObject :: DecodeState BS.ByteString KBO.KBObject
readObject = do
    byte <- ensureGet 1 >>= (\x -> return $ BS.index x 0)
    let head = KBH.fromByte byte
        t = KBH.headerType head
    case t of
        KBH.HeaderTerminator -> return KBO.KBEmptyArray
        KBH.HeaderString -> readString head
        KBH.HeaderInteger -> readInt head
        KBH.HeaderNULL -> readNull head
        KBH.HeaderArray -> readArray head
        KBH.HeaderDictionary -> readHash head
        KBH.HeaderData -> readData head
        KBH.HeaderFloat -> KeyedBits.Decode.readFloat head

readString :: KBH.Header -> DecodeState BS.ByteString KBO.KBObject
readString _ = do
    bytes <- readNULLTerminated BS.empty
    let str = map (chr . fromIntegral) $ BS.unpack bytes
    return $ KBO.KBString str

readInt :: KBH.Header -> DecodeState BS.ByteString KBO.KBObject
readInt (KBH.Header _ l _) = do
    let len = if l > 1 then 8 else 4
    bytes <- ensureGet len
    let number = KBI.decodeInt bytes
    return $ KBO.KBInt number

readNull :: KBH.Header -> DecodeState BS.ByteString KBO.KBObject
readNull _ = return KBO.KBNull

readData :: KBH.Header -> DecodeState BS.ByteString KBO.KBObject
readData (KBH.Header _ l _) = do
    let lenLen = fromIntegral l
    bytes <- ensureGet (lenLen + 1)
    let dataLen = KBI.decodeWord bytes
    contents <- ensureGet (fromIntegral dataLen)
    return $ KBO.KBData $ BS.unpack contents

readFloat :: KBH.Header -> DecodeState BS.ByteString KBO.KBObject
readFloat _ = do
    bytes <- readNULLTerminated BS.empty
    let str = map (chr . fromIntegral) $ BS.unpack bytes
        num = read str :: Float
    return $ KBO.KBFloat num

readArray :: KBH.Header -> DecodeState BS.ByteString KBO.KBObject
readArray t = do
    obj <- readObject
    if obj == KBO.KBEmptyArray then return obj
    else do nextObj <- readArray t
            return $ KBO.KBArray obj nextObj

readHash :: KBH.Header -> DecodeState BS.ByteString KBO.KBObject
readHash t = do
        key <- dictKey
        if length key == 0 then return KBO.KBEmptyHash
        else do
            obj <- readObject
            nextObj <- readHash t
            return $ KBO.KBHash key obj nextObj

dictKey :: DecodeState BS.ByteString String
dictKey = do
    buffer <- ensureGet 1
    let byte = BS.index buffer 0
        clean = if byte .&. 0x80 == 0 then byte else byte `xor` 0x80
        letter = chr $ fromIntegral clean
    if byte == 0 || byte /= clean
    then let next = if byte /= clean then [letter] else []
         in return next
    else do next <- dictKey
            return (letter:next)

readNULLTerminated :: BS.ByteString -> DecodeState BS.ByteString BS.ByteString
readNULLTerminated s = do
    byte <- ensureGet 1 >>= (\x -> return $ BS.index x 0)
    if byte == 0 then return s
    else let buff = s `BS.append` (BS.singleton byte)
         in readNULLTerminated buff

ensureGet :: Int -> DecodeState BS.ByteString BS.ByteString
ensureGet n = DecodeState $ \bs ->
    if BS.length bs < fromIntegral n then throw BufferUnderflowException
    else BS.splitAt (fromIntegral n) bs
