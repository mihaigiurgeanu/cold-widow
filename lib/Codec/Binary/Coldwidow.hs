module Codec.Binary.Coldwidow (encode, decode, packInteger, unpackInteger) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Word (Word8)
import Numeric (showIntAtBase)


chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
         'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
         'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
         'U', 'V', 'W', 'X', 'Y', 'Z', ' ', '$', '%', '*',
         '+', '-', '.', '/', ':']

base = 45

-- external inteface
encode :: Integer -> String
encode x = showIntAtBase base toDigit x ""

packInteger :: ByteString -> Integer
packInteger s = packInteger' (B.unpack s) 0

decode :: String -> Integer
decode _ = 0

unpackInteger :: Integer -> ByteString
unpackInteger = undefined

-- internal functions

toDigit :: Int -> Char 
toDigit i = chars !! i -- TODO: make a more efficient implementation

packInteger' :: [Word8] -> Integer -> Integer
packInteger' [] result = result
packInteger' (d:ds) result = packInteger' ds ((fromIntegral d) .|. (result `shiftL` 8))

