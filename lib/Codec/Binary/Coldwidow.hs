{- |
Module:         Codec.Binary.Coldwidow
Description:    Base45 encoding/decoding 

QR Code alphanumeric mode accepts a set of 45 characters. This module offers
functions to encode\/decode binary data to\/from text representation using
only the 45 characters allowed by the qr-code alphanumeric mode.
-}
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

-- | Encodes binary data into a 'String'. The resulting string will contain
-- only the 45 characters allowed by the qr-code alphanumeric mode. The
-- binary data to be encoded should be represented as a 'Integer'. To
-- convert a 'ByteString' to an 'Integer' you can use the 'packInteger'
-- function defined bellow.
encode :: Integer -> String
encode x = showIntAtBase base toDigit x ""

-- | Converts a 'ByteString' to an 'Integer'. This is used to represent binary
-- data as an 'Integer' that can be passed to 'encode' function defined above.
packInteger :: ByteString -> Integer
packInteger s = packInteger' (B.unpack s) 0

-- | Decodes binary data from its text representation. The text representation of
-- data, obtained with the 'encode' function defined above, will contain only
-- characters allowed in the qr-code alphanumeric mode. The decoded binary data
-- returned by this function will be represented as an 'Integer'. To convert
-- it into a 'ByteString' you can use the 'unpackInteger' function defined bellow.
decode :: String -> Integer
decode encoded = let parsed = readEncodedInt encoded
                 in case parsed of
                      [(val, [])] -> val
                      otherwise -> error $ "Error parsing encoded value."++parseMessages
                        where
                          parseMessages = concatMap parseMessage parsed

-- | Converts an 'Integer' into a 'ByteString'. The Integer holds the binary
-- represantation of the data you obtain by using the 'decode' function defined
-- above. You can use the 'ByteString' represantation to, for example, save the
-- binary data into a file.
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

