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

unpackHash :: KBObject -> [(String, KBObject)]
unpackHash KBEmpty = []
unpackHash (KBHash k v n) = (k,v) : (unpackHash n)
unpackHash _ = []

unpackArray :: KBObject -> [KBObject]
unpackArray KBEmpty = []
unpackArray (KBArray v n) = v : (unpackArray n)

isEmpty :: KBObject -> Bool
isEmpty KBEmpty = True
isEmpty _ = False
