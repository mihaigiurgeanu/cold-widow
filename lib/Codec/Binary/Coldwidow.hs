module Codec.Binary.Coldwidow (encode, decode, packInteger, unpackInteger) where

import Data.Bits (shiftL, (.|.), shiftR)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Word (Word8)
import Numeric (showIntAtBase, readInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust)


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
decode encoded = let parsed = readEncodedInt encoded
                 in case parsed of
                      [(val, [])] -> val
                      otherwise -> error $ "Error parsing encoded value."++parseMessages
                        where
                          parseMessages = concatMap parseMessage parsed

unpackInteger :: Integer -> ByteString
unpackInteger 0 = B.singleton 0
unpackInteger x = B.pack $ unpackInteger' x []

-- internal functions

toDigit :: Int -> Char 
toDigit i = chars !! i -- TODO: make a more efficient implementation

readEncodedInt :: ReadS Integer
readEncodedInt = readInt base isDigit fromDigit

isDigit :: Char -> Bool
isDigit digit = isJust $ elemIndex digit chars

fromDigit :: Char -> Int
fromDigit digit = fromJust $ elemIndex digit chars

packInteger' :: [Word8] -> Integer -> Integer
packInteger' [] result = result
packInteger' (d:ds) result = packInteger' ds ((fromIntegral d) .|. (result `shiftL` 8))

unpackInteger' :: Integer -> [Word8] -> [Word8]
unpackInteger' 0 result = result
unpackInteger' value result = unpackInteger' (value `shiftR` 8) $ (fromInteger value :: Word8) : result

parseMessage :: (Integer, String) -> String
parseMessage (parsed, remainder) = "\n\tStopped at: " ++ (take 20 remainder)

