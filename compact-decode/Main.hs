-- compact-decode.hs
-- A small variant of decode, meant to have very small source code

module Main where {
import System.Environment (getArgs);
import qualified Data.ByteString.Lazy as B;
import System.IO (withBinaryFile, IOMode(..));
import Data.Word (Word8);
import Numeric (readInt);
import Data.Maybe (fromJust, isJust);
import Data.List (elemIndex);
import Data.Bits (shiftR);
main = do {
as <- getArgs;
let {fn = as!!0};
t <- getContents;
let {(z, nz)=span (=='0') t};
withBinaryFile fn WriteMode (\h -> do {
                                  mapM_ (\_ -> B.hPut h (B.singleton 0)) z;
                                  if null nz then return () else B.hPut h $ ui $ dcd nz;
                            });
};
cs = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9','A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J','K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T','U', 'V', 'W', 'X', 'Y', 'Z', ' ', '$', '%', '*','+', '-', '.', '/', ':'];
b = 45;
dcd e = let {p = rdi e} in case p of {
                         [(val, [])] -> val;
                         otherwise -> error $ "Parse error at: " ++ (take 10 $ snd $ p !! 0);
                        };
ui 0 = B.singleton 0;
ui x = B.pack $ ui' x [];
rdi = readInt b iD fD;iD d = isJust $ elemIndex d cs;
fD d = fromJust $ elemIndex d cs;
ui' 0 r = r;
ui' v r = ui' (v `shiftR` 8) $ (fromInteger v :: Word8) : r;
}
