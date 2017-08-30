-- write-qr executable
module Mainn (main) where

import Codec.Binary.QRCode (encode, toArray, version, Version, Mode(..), ErrorLevel(..))

import Data.List (find)

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
            (1853, (23, L)),
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
  
