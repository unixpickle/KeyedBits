module KeyedBits.Read (
    readObject,
    ReadException(..)
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
import System.IO
import System.IO.Error

data ReadException = BufferUnderflowException deriving (Show, Typeable)
instance Exception ReadException

readObject :: Handle -> IO KBO.KBObject
readObject h = do
    byte <- ensureGet 1 h >>= (\x -> return $ BS.index x 0)
    putStrLn "got byte"
    let head = KBH.fromByte byte
        t = KBH.headerType head
    case t of
        KBH.HeaderTerminator -> return KBO.KBEmpty
        KBH.HeaderString -> readString head h
        KBH.HeaderInteger -> readInt head h
        KBH.HeaderNULL -> readNull head h
        KBH.HeaderArray -> readArray head h
        KBH.HeaderDictionary -> readHash head h
        KBH.HeaderData -> readData head h
        KBH.HeaderFloat -> KeyedBits.Read.readFloat head h

readString :: KBH.Header -> Handle -> IO KBO.KBObject
readString _ h = do
    bytes <- readNULLTerminated BS.empty h
    let str = map (chr . fromIntegral) $ BS.unpack bytes
    return $ KBO.KBString str

readInt :: KBH.Header -> Handle -> IO KBO.KBObject
readInt (KBH.Header _ l _) h = do
    let len = if l > 1 then 8 else 4
    bytes <- ensureGet len h
    let number = KBI.decodeInt bytes
    return $ KBO.KBInt number

readNull :: KBH.Header -> Handle -> IO KBO.KBObject
readNull _ _ = return KBO.KBNull

readData :: KBH.Header -> Handle -> IO KBO.KBObject
readData (KBH.Header _ l _) h = do
    let lenLen = fromIntegral l
    bytes <- ensureGet (lenLen + 1) h
    let dataLen = KBI.decodeWord bytes
    contents <- ensureGet (fromIntegral dataLen) h
    return $ KBO.KBData $ BS.unpack contents

readFloat :: KBH.Header -> Handle -> IO KBO.KBObject
readFloat _ h = do
    bytes <- readNULLTerminated BS.empty h
    let str = map (chr . fromIntegral) $ BS.unpack bytes
        num = read str :: Float
    return $ KBO.KBFloat num

readArray :: KBH.Header -> Handle -> IO KBO.KBObject
readArray t h = do
    obj <- readObject h
    if obj == KBO.KBEmpty then return obj
    else do nextObj <- readArray t h
            return $ KBO.KBArray obj nextObj

readHash :: KBH.Header -> Handle -> IO KBO.KBObject
readHash t h = do
        key <- dictKey h
        if length key == 0 then return KBO.KBEmpty
        else do
            obj <- readObject h
            nextObj <- readHash t h
            return $ KBO.KBHash key obj nextObj

dictKey :: Handle ->IO String
dictKey h = do
    buffer <- ensureGet 1 h
    let byte = BS.index buffer 0
        clean = if byte .&. 0x80 == 0 then byte else byte `xor` 0x80
        letter = chr $ fromIntegral clean
    if byte == 0 || byte /= clean
    then let next = if byte /= clean then [letter] else []
         in return next
    else do next <- dictKey h
            return (letter:next)

readNULLTerminated :: BS.ByteString -> Handle -> IO BS.ByteString
readNULLTerminated s h = do
    byte <- ensureGet 1 h >>= (\x -> return $ BS.index x 0)
    if byte == 0 then return s
    else let buff = s `BS.append` (BS.singleton byte)
         in readNULLTerminated buff h

ensureGet :: Int -> Handle -> IO BS.ByteString
ensureGet n h = do
    b <- BS.hGet h n
    putStrLn "got buffer"
    if BS.null b || BS.length b /= fromIntegral n
    then throwIO BufferUnderflowException
    else return b
