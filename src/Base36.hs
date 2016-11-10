-- Author:  Christophe De Troyer
-- Email:   christophe.detroyer@gmail.com
-- License: GPLv3
-- Date:    November 10, 2016

module Base36 (base36) where
import Data.Word

-- Based on https://en.wikipedia.org/wiki/Base36#C.23_implementation

chars :: String
chars = ['0','1','2','3','4','5','6','7','8','9','A',
         'B','C','D','E','F','G','H','I','J','K','L',
         'M','N','O','P','Q','R','S','T','U','V','W',
         'X','Y','Z']

base36 :: Word32 -> String
base36 w = foldl conv "" (slices w [])
           where conv res x   = (chars !! fromIntegral x):res
                 slices word res = let s     = word `mod` 36
                                       word' = word `div` 36
                                in
                                  if word' > 0
                                     then slices word' (res ++ [s])
                                     else res ++ [s]
