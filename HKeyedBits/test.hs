import qualified KeyedBits.Encode as KBE
import qualified KeyedBits.Object as KB
import qualified KeyedBits.Decode as KBD
import System.IO
import System.Environment
import Data.ByteString
import qualified Data.ByteString.Lazy as Lazy

main = do
    let hash = KB.packHash [("count", KB.KBInt 10),("name", KB.KBString "alex")]
        obj1 = [KB.KBString "test", KB.KBInt (-2^32)]
        obj2 = [KB.KBNull, KB.KBString "balls"]
        obj = KB.packArray $ hash : (obj1 ++ obj2)
        encoded = KBE.encode obj
    withBinaryFile "/Users/alex/Desktop/test.txt" WriteMode (flip hPut encoded)
    let str = "test"
        enc = KBE.encode $ obj
        dec = KBD.decode $ Lazy.pack $ unpack enc
    System.IO.putStrLn $ show dec
