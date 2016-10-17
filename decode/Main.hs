-- File: decode/Main.hs

module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B

import Codec.Binary.Coldwidow

main :: IO ()
main = do 
  args <- getArgs
  let fileName = args !! 0
  encodedText <- getContents
  writeIntegerToFile fileName $ decode encodedText

writeIntegerToFile :: FilePath -> Integer -> IO ()
writeIntegerToFile fileName x = B.writeFile fileName $ unpackInteger x


