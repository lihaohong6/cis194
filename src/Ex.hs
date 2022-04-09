module Ex where

import Data.Char (chr)


carpet :: Int -> String
carpet n =
  let row = reverse [0..n] ++ [1..n]
   in unlines
        ( map
          (\x -> map (\y -> chr (48 + max x y)) row)
          row
        )
