module KeyedBits.Object (
    KBObject(..),
    packHash,
    packArray,
    unpackHash,
    unpackArray
) where

import Data.Word

data KBObject = KBString String | KBInt Int | KBFloat Float | KBArray KBObject KBObject | KBHash String KBObject KBObject | KBData [Word8] | KBNull | KBEmpty deriving (Show, Eq)

packHash :: [(String, KBObject)] -> KBObject
packHash [] = KBEmpty
packHash ((x,y):xs) = KBHash x y $ packHash xs

packArray :: [KBObject] -> KBObject
packArray [] = KBEmpty
packArray (x:xs) = KBArray x $ packArray xs

unpackHash :: KBObject -> Maybe [(String, KBObject)]
unpackHash KBEmpty = Just []
unpackHash (KBHash k v n) = unpackHash n >>= (\x -> Just $ (k, v):x)
unpackHash _ = Nothing

unpackArray :: KBObject -> Maybe [KBObject]
unpackArray KBEmpty = Just []
unpackArray (KBArray v n) = unpackArray n >>= (\x -> Just $ v:x)
unpackArray _ = Nothing

unpackString :: KBObject -> Maybe String
unpackString (KBString s) = Just s
unpackString _ = Nothing

unpackInt :: KBObject -> Maybe Int
unpackInt (KBInt i) = Just i
unpackInt _ = Nothing

unpackFloat :: KBObject -> Maybe Float
unpackFloat (KBFloat f) = Just f
unpackFloat _ = Nothing

unpackData :: KBObject -> Maybe [Word8]
unpackData (KBData b) = Just b
unpackData _ = Nothing

isEmpty :: KBObject -> Bool
isEmpty KBEmpty = True
isEmpty _ = False
