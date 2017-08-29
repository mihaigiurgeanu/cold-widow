{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

import Control.Monad.State (StateT, get, put)
import Control.Monad.IO.Class (liftIO)

import Data.Word (Word8)

-- | opaque data structure used by quirc library
data Quirc = Quirc

foreign import ccall "quirc_new"        quirc_new       :: IO (Ptr Quirc)
foreign import ccall "quirc_destroy"    quirc_destroy   :: Ptr Quirc -> IO ()
foreign import ccall "quirc_resize"     quirc_resize    :: Ptr Quirc -> Int -> Int -> IO Int
foreign import ccall "quirc_count"      quirc_count     :: Ptr Quirc -> IO Int
foreign import ccall "extract_text"     extract_text    :: Ptr Quirc -> Int -> Ptr CString -> IO Int

-- | contains the pointer to the decoder and the current width and
-- height of the image buffer associated with the decoder
data Decoder = Decoder { quirc  :: (Ptr Quirc),
                         width  :: Int,
                         height :: Int }

data Image = Image (Ptr Word8) Int Int

-- | checks if the current buffer needs to be resized, and, if
-- it does, calls 'quirc_resize'
resizeBuffer :: Int -> Int -> StateT Decoder IO ()
resizeBuffer w h = do
  decoder <- get
  if w /= width decoder || h /= height decoder
    then do put $ decoder { width = w, height = h }
            liftIO $ do
              putStrLn $ "Resizing quirc buffer to (" ++ (show w) ++ ", " ++ (show h) ++ ")"
              result <- quirc_resize (quirc decoder) w h
              putStrLn $ "Resizing operation returned " ++ (show result)
    else liftIO $ putStrLn $ "Using current buffer of (" ++ (show $ width decoder) ++ ", " ++ (show $ height decoder) ++ ")"
  
-- | reads one qr code using the currently allocated quirc buffer. It does
-- resize the buffer if it needs to
readQR :: Image -> StateT Decoder IO [Either String String]
readQR (Image bits w h)  = do
  resizeBuffer w h
  liftIO $ feedImage bits
  decoder <- get
  codesCount <- liftIO $ quirc_count $ quirc decoder
  liftIO $ sequence $ extractCodes codesCount 0 (quirc decoder)

-- | extract all the codes in an image
extractCodes :: Int -> Int -> Ptr Quirc -> [IO (Either String String)]
extractCodes n i qr | i>= n = []
                    | otherwise = let thisCode =
                                        alloca (\ p_text -> let
                                                   readText = peek p_text >>= peekCString
                                                   in
                                                     do
                                                       result <- extract_text qr i p_text
                                                       case result of 0 -> Left <$> readText
                                                                      1 -> Right <$> readText
                                                                      _ -> return $ Right $ "Unknown result returned by extract_text function: " ++ (show result))
                                  in
                                    thisCode : (extractCodes n (i+1) qr)
                    
feedImage :: Ptr Word8 -> IO ()
feedImage _ = return ()

main :: IO ()
main =
  do
    qr <- quirc_new
    if qr /= nullPtr
      then putStrLn "quirc strcture allocated"
      else fail "quirc structure allocation FAILED"
    quirc_destroy qr
