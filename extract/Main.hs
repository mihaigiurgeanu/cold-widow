{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.Ptr (Ptr, nullPtr)

-- | opaque data structure used by quirc library
data Quirc = Quirc

foreign import ccall "quirc_new" quirc_new :: IO (Ptr Quirc)
foreign import ccall "quirc_destroy" quirc_destroy :: Ptr Quirc -> IO ()

main :: IO ()
main =
  do
    qr <- quirc_new
    if qr /= nullPtr
      then putStrLn "quirc strcture allocated"
      else fail "quirc structure allocation FAILED"
    quirc_destroy qr
