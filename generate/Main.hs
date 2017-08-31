-- write-qr executable
module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import Codec.Binary.QRCode (encode, toArray, version, Version, Mode(..), ErrorLevel(..))
import Codec.Picture (Image, Pixel8, generateImage, writePng)
import Codec.Binary.Coldwidow (isDigit)

import Data.List (find)
import Data.Word (Word8)
import Data.Array (Array, bounds, (!))

qrConfig :: [(Int, (Int, ErrorLevel))]
qrConfig = [(  10, ( 1, H)),
            (  16, ( 1, Q)),
            (  20, ( 1, M)),
            (  25, ( 1, L)),
            (  29, ( 2, Q)),
            (  38, ( 2, M)),
            (  47, ( 2, L)),
            (  61, ( 3, M)),
            (  77, ( 3, L)),
            (  90, ( 4, M)),
            ( 114, ( 4, L)),
            ( 122, ( 5, M)),
            ( 154, ( 5, L)),
            ( 195, ( 6, L)),
            ( 224, ( 7, L)),
            ( 279, ( 8, L)),
            ( 335, ( 9, L)),
            ( 395, (10, L)),
            ( 468, (11, L)),
            ( 535, (12, L)),
            ( 619, (13, L)),
            ( 667, (14, L)),
            ( 854, (16, L)),
            ( 938, (17, L)),
            (1046, (18, L)),
            (1153, (19, L)),
            (1249, (20, L)),
            (1352, (21, L)),
            (1460, (22, L)),
            (1588, (23, L)),
            (1704, (24, L)),
            (1853, (25, L)),
            (1990, (26, L)),
            (2132, (27, L)),
            (2223, (28, L)),
            (2369, (29, L)),
            (2520, (30, L)),
            (2677, (31, L)),
            (2840, (32, L)),
            (3009, (33, L)),
            (3183, (34, L)),
            (3351, (35, L)),
            (3537, (36, L)),
            (3729, (37, L)),
            (3927, (38, L)),
            (4087, (39, L)),
            (4296, (40, L))]
           
selectConfig :: Int -> Maybe (Version, ErrorLevel)
selectConfig size = do
  (_, (ver, lvl)) <- find (\ (maxsz, _) -> maxsz >= size) qrConfig
  ver' <- version ver
  return (ver', lvl)
  
generateQR :: String -> Maybe (Array (Int, Int) Word8)
generateQR text = do
  (v, l) <- selectConfig $ length text
  m <- encode v l Alphanumeric text
  return $ toArray m

generateQRImage :: String -> Maybe (Image Pixel8)
generateQRImage text = do
  bits <- generateQR text
  let ((x0, y0), (x1, y1)) = bounds bits
      scale = 2
  return $ generateImage (\ x y -> bits ! ((x `div` scale) + x0, (y `div` scale) + y0)) (scale*(x1-x0+1)) (scale*(y1-y0+1))


checkChar :: Char -> IO ()
checkChar c = if isDigit c
              then return ()
              else hPutStrLn stderr $ "Character " ++ (show c) ++ " is not allowed"
                   
checkConfig :: String -> IO ()
checkConfig text = do
  hPutStrLn stderr $ ">>>>>>>>" ++ (show text) ++ "<<<<<<<<<"
  let ltext = length text
  hPutStrLn stderr $ "Length of text is " ++ (show ltext)
  mapM_ checkChar text
  case selectConfig ltext of Nothing -> hPutStrLn stderr "No configuration could be selected for the text length"
                             Just conf -> hPutStrLn stderr $ "Using configuration " ++ (show conf)

  let qr = generateQR text
  case qr of Nothing -> hPutStrLn stderr "Generation of QR fails"
             Just qr' -> hPutStrLn stderr $ "Array bounds: " ++ (show $ bounds qr')

main :: IO ()
main = do
  args <- getArgs
  let fileName = args !! 0
  text <- getContents  
  let text' = filter (\c -> c /= '\n' && c /= '\r') text
  checkConfig text'
  let image' = generateQRImage text'
  case image' of Nothing   -> fail "The text cannot be encoded. Make sure the text is alphanumeric encoded (use encode45) and the length of the text is less 4296 characters"
                 Just image -> writePng fileName image
