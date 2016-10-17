-- File: decode/Main.hs

module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import System.IO (withBinaryFile, IOMode(..))
import Codec.Binary.Coldwidow

main :: IO ()
main = do 
  args <- getArgs
  let fileName = args !! 0
  encodedText <- getContents
  let (zeroes, nonzeroes) = span (=='0') encodedText
  withBinaryFile
    fileName
    WriteMode
    (\h -> do
        mapM_ (\_ -> B.hPut h (B.singleton 0)) zeroes
        if null nonzeroes
          then return ()
          else B.hPut h $ unpackInteger $ decode nonzeroes)




