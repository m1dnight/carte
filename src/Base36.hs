module Base36 (base36) where
import Data.Word

-- Based on https://en.wikipedia.org/wiki/Base36#C.23_implementation

chars = ['0','1','2','3','4','5','6','7','8','9','A',
         'B','C','D','E','F','G','H','I','J','K','L',
         'M','N','O','P','Q','R','S','T','U','V','W',
         'X','Y','Z']

base36 :: Word32 -> String
base36 w = foldl conv "" (slices w [])
           where conv res x = (chars !! (fromIntegral x)):res
                 slices w res = let s  = w `mod` 36
                                    w' = w `div` 36
                                in
                                  if w' > 0
                                     then slices w' (res ++ [s])
                                     else (res ++ [s])
