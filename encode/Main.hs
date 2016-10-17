-- File: encode/Main.hs

module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B

import Codec.Binary.Coldwidow

main :: IO ()
main = do 
  args <- getArgs
  let fileName = args !! 0
  bytes <- B.readFile fileName
  let (zeroes, nonzeroes) = B.span (==0) bytes
  mapM_ (\_ -> putStr "0") (B.unpack zeroes)
  if B.null nonzeroes
    then return ()
    else putStr $ encode $ packInteger nonzeroes

