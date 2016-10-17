module Main where

import System.Environment (getArgs)
import Data.ByteString.Lazy (ByteString)
import Data.Bits (shiftL, (.|.))
import Data.Word (Word8)
import Numeric (showIntAtBase)

import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do 
  args <- getArgs
  let fileName = args !! 0
  bigval <- readFileAsInteger fileName
  putStr $ encode bigval

chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
         'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
         'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
         'U', 'V', 'W', 'X', 'Y', 'Z', ' ', '$', '%', '*',
         '+', '-', '.', '/', ':']

base = 45

encode :: Integer -> String
encode x = showIntAtBase base toDigit x ""

toDigit i = chars !! i -- TODO: make a more efficient implementation

readFileAsInteger :: FilePath -> IO Integer
readFileAsInteger fileName = B.readFile fileName  >>= return . packInteger

packInteger :: ByteString -> Integer
packInteger s = packInteger' (B.unpack s) 0

packInteger' :: [Word8] -> Integer -> Integer
packInteger' [] result = result
packInteger' (d:ds) result = packInteger' ds ((fromIntegral d) .|. (result `shiftL` 8))
