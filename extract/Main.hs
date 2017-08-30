{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek, pokeByteOff)

import Control.Monad.State (StateT, get, put, gets, evalStateT)
import Control.Monad.IO.Class (liftIO)

import Data.Word (Word8)
import Data.Bits (shiftR)

import qualified Codec.Picture as I

import System.Environment (getArgs)

-- | opaque data structure used by quirc library
data Quirc = Quirc

foreign import ccall "quirc_new"        quirc_new       :: IO (Ptr Quirc)
foreign import ccall "quirc_destroy"    quirc_destroy   :: Ptr Quirc -> IO ()
foreign import ccall "quirc_resize"     quirc_resize    :: Ptr Quirc -> Int -> Int -> IO Int
foreign import ccall "quirc_count"      quirc_count     :: Ptr Quirc -> IO Int
foreign import ccall "extract_text"     extract_text    :: Ptr Quirc -> Int -> Ptr CString -> IO Int
foreign import ccall "quirc_begin"      quirc_begin     :: Ptr Quirc -> Ptr CInt -> Ptr CInt -> IO (Ptr Word8)
foreign import ccall "quirc_end"        quirc_end       :: Ptr Quirc -> IO ()

-- | contains the pointer to the decoder and the current width and
-- height of the image buffer associated with the decoder
data Decoder = Decoder { quirc  :: (Ptr Quirc),
                         width  :: Int,
                         height :: Int }

class QRImage a where
  -- | the width of the image in pixels
  imageWidth  :: a -> Int
  -- | the height of the image in pixels
  imageHeight :: a -> Int
  -- | the 8 bit grey value of the pixels at x, y coordinates, where
  -- x is the horizontal position and has values between 0 and (imageWidth-1)
  -- and y is the vertical position and has values between 0 and
  -- (imageHeight - 1)
  greyValue   ::
    a           -- ^ the image
    -> Int      -- ^ x
    -> Int      -- ^ y
    -> Word8    -- ^ gray value of the pixel

-- | copy image pixels into the decoder's buffer
feedImage :: QRImage a => a -> StateT Decoder IO ()
feedImage img = do
  qr <- gets quirc
  (pbuf, w, h) <- liftIO $
    alloca ( \ p_w -> alloca ( \ p_h -> do
                                 pbuf <- quirc_begin qr p_w p_h
                                 w <- peek p_w
                                 h <- peek p_h
                                 return (pbuf, fromIntegral w, fromIntegral h)))
  liftIO $ sequence_ [ pokeByteOff pbuf (atx + w * aty) v | atx <- [0..(w-1)], aty <- [0..(h-1)], let v = greyValue img atx aty ]
  liftIO $ quirc_end qr

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
readQR :: QRImage a => a -> StateT Decoder IO [Either String String]
readQR image  = do
  resizeBuffer (imageWidth image) (imageHeight image)
  feedImage image
  decoder <- get
  codesCount <- liftIO $ quirc_count $ quirc decoder
  liftIO $ putStrLn $ (show codesCount) ++ " code(s) detected in the file"
  liftIO $ sequence $ [extractCode i (quirc decoder)| i <- [0..(codesCount - 1)]]

-- | extract all the codes in an image
extractCode :: Int -> Ptr Quirc -> IO (Either String String)
extractCode i qr = alloca (\ p_text -> let
                              readText = peek p_text >>= peekCString
                              in
                                do
                                  result <- extract_text qr i p_text
                                  case result of 0 -> Right <$> readText
                                                 1 -> Left <$> readText
                                                 _ -> return $ Right $ "Unknown result returned by extract_text function: " ++ (show result))
                 
instance QRImage I.DynamicImage where
  imageWidth (I.ImageY8     img) = I.imageWidth img
  imageWidth (I.ImageY16    img) = I.imageWidth img
  imageWidth (I.ImageYF     img) = I.imageWidth img
  imageWidth (I.ImageYA8    img) = I.imageWidth img
  imageWidth (I.ImageYA16   img) = I.imageWidth img
  imageWidth (I.ImageRGB8   img) = I.imageWidth img
  imageWidth (I.ImageRGB16  img) = I.imageWidth img
  imageWidth (I.ImageRGBF   img) = I.imageWidth img
  imageWidth (I.ImageRGBA8  img) = I.imageWidth img
  imageWidth (I.ImageRGBA16 img) = I.imageWidth img
  imageWidth (I.ImageYCbCr8 img) = I.imageWidth img
  imageWidth (I.ImageCMYK8  img) = I.imageWidth img
  imageWidth (I.ImageCMYK16 img) = I.imageWidth img

  imageHeight (I.ImageY8     img) = I.imageHeight img
  imageHeight (I.ImageY16    img) = I.imageHeight img
  imageHeight (I.ImageYF     img) = I.imageHeight img
  imageHeight (I.ImageYA8    img) = I.imageHeight img
  imageHeight (I.ImageYA16   img) = I.imageHeight img
  imageHeight (I.ImageRGB8   img) = I.imageHeight img
  imageHeight (I.ImageRGB16  img) = I.imageHeight img
  imageHeight (I.ImageRGBF   img) = I.imageHeight img
  imageHeight (I.ImageRGBA8  img) = I.imageHeight img
  imageHeight (I.ImageRGBA16 img) = I.imageHeight img
  imageHeight (I.ImageYCbCr8 img) = I.imageHeight img
  imageHeight (I.ImageCMYK8  img) = I.imageHeight img
  imageHeight (I.ImageCMYK16 img) = I.imageHeight img
  
  greyValue (I.ImageY8     img) x y = I.pixelAt img x y
  greyValue (I.ImageY16    img) x y = fromIntegral $ (I.pixelAt img x y) `shiftR` 8
  greyValue (I.ImageYF     img) x y = round $ (I.pixelAt img x y) * 255
  greyValue (I.ImageYA8    img) x y = pixelYABtoGrey $ I.pixelAt img x y
    where pixelYABtoGrey (I.PixelYA8 y _) = y
  greyValue (I.ImageYA16   img) x y = pixelYA16toGrey $ I.pixelAt img x y
    where pixelYA16toGrey (I.PixelYA16 y _) = fromIntegral $ y `shiftR` 8
  greyValue (I.ImageRGB8   img) x y = rgbavg $ I.pixelAt img x y
    where rgbavg (I.PixelRGB8 r g b) = (r + g + b) `div` 3
  greyValue (I.ImageRGB16  img) x y = fromIntegral $ (rgbavg $ I.pixelAt img x y) `shiftR` 8
    where rgbavg (I.PixelRGB16 r g b) = (r + g + b) `div` 3
  greyValue (I.ImageRGBF   img) x y = round $ (rgbavg $ I.pixelAt img x y) * 255
    where rgbavg (I.PixelRGBF r g b) = (r + g + b) / 3
  greyValue (I.ImageRGBA8  img) x y = rgbavg $ I.pixelAt img x y
    where rgbavg (I.PixelRGBA8 r g b _) = (r + g + b) `div` 3
  greyValue (I.ImageRGBA16 img) x y = fromIntegral $ (rgbavg $ I.pixelAt img x y) `shiftR` 8
    where rgbavg (I.PixelRGBA16 r g b _) = (r + g + b) `div` 3
  greyValue (I.ImageYCbCr8 img) x y = luminance $ I.pixelAt img x y
    where luminance (I.PixelYCbCr8 y _ _) = y
  greyValue (I.ImageCMYK8  img) x y = rgbavg $ I.pixelAt img x y
    where rgbavg (I.PixelCMYK8 c m y k) = let
            (r, g, b) = (255 - c', 255 - m', 255 - y')
            c' = c * (255 - k) `div` 255 + k
            m' = m * (255 - k) `div` 255 + k
            y' = y * (255 - k) `div` 255 + k
            in
              (r + g + b) `div` 3
  greyValue (I.ImageCMYK16 img) x y = fromIntegral $ (rgbavg $ I.pixelAt img x y) `shiftR` 8
    where rgbavg (I.PixelCMYK16 c m y k) = let
            (r, g, b) = (255 - c', 255 - m', 255 - y')
            c' = c * (255 - k) `div` 255 + k
            m' = m * (255 - k) `div` 255 + k
            y' = y * (255 - k) `div` 255 + k
            in
              (r + g + b) `div` 3

main :: IO ()
main =
  do
    files <- getArgs

    qr <- quirc_new
    if qr /= nullPtr
      then putStrLn "quirc strcture allocated"
      else fail "quirc structure allocation FAILED"
    let extractQRs' = sequence_ $ map extractQRs files
    evalStateT extractQRs' (Decoder qr 0 0)
    quirc_destroy qr

-- | extract text from the qr codes contained in an image file
-- and displays the result on the standard output
extractQRs :: String -> StateT Decoder IO ()
extractQRs filePath = do
  img <- liftIO $ I.readImage filePath
  case img of Left err -> liftIO $ putStrLn $ filePath ++ " - error: " ++ err
              Right img' -> do liftIO $ putStrLn filePath
                               texts <- readQR img'
                               liftIO $ sequence_ $ map displayText texts

-- | displays either the text read from a QR code or the error reading the QR code
displayText :: Either String String -> IO ()
displayText msg = case msg of Left  msg' -> putStrLn $ "[[[[[" ++ msg' ++ "]]]]]"
                              Right msg' -> do putStrLn ">>>>>>>>>>>"
                                               putStrLn msg'
                                               putStrLn "<<<<<<<<<<<"

