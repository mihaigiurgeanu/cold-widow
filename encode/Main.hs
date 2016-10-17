module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B

import Codec.Binary.Coldwidow

main :: IO ()
main = do 
  args <- getArgs
  let fileName = args !! 0
  bigval <- readFileAsInteger fileName
  putStr $ encode bigval

readFileAsInteger :: FilePath -> IO Integer
readFileAsInteger fileName = B.readFile fileName  >>= return . packInteger

